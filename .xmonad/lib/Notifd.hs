{-# LANGUAGE OverloadedStrings #-}

-- Notification daemon for XMonad
--
-- Requires XMonad to be compiled with the -threaded flag, witch may cause
-- problems with XMonad and/or other contrib modules.

module Notifd
  ( Notif(..)
  , NotifLog
  , NotifUpdateTimer(..)
  , NotifConf(..)
  , defaultNotifConf
  , Daemon(..)
  , runNotifDaemon
  ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception.Base (throwIO)

import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Data.List (intercalate)
import Data.Maybe
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Map as M

import DBus
import DBus.Client


type NotifLog = M.Map Word32 String


-- | Notification data structure
data Notif = Notif
  { notifId  :: Word32
  , appName  :: String
  , appIcon  :: String
  , summary  :: String
  , body     :: String
  , actions  :: [String]
  , hints    :: M.Map String Variant
  , duration :: Int32
  } deriving (Show)


-- | Timer type used to control the rendering of the status bar
data NotifUpdateTimer = ClockSyncMinutes
                        -- ^ Update once every minute in sync with the system clock

                      | ClockSyncSeconds
                        -- ^ Update once every second in sync with the system clock

                      | Every Int
                        -- ^ Update once every n micro seconds

                      | Once
                        -- ^ Only update once


data NotifConf = NotifConf
  { notifDuration    :: Int
    -- ^ The duration a notification will be displayed (in micro seconds) if no
    -- duration is specified by the sender

  , notifFormat      :: Notif -> String
    -- ^ Formatting function for notifications

  , notifFormatLog   :: Maybe (Notif -> String)
    -- ^ Formatting function for log entries, if nothing the `notifFormat`
    -- function will be used in its stead

  , notifPlay        :: Maybe FilePath -> IO ()
    -- ^ Function for playing sound file. An input of Nothing indicates that the
    -- sender did not specify a sound-file... perhaps useful if you want to play
    -- a default sound. Note also that if the `suppress-sound` hint was
    -- specified this function is not called.

  , notifUpdateTimer :: NotifUpdateTimer
    -- ^ Update timer for the status bar

  , notifStatusbar   :: NotifLog -> IO String
    -- ^ Formatting function for the status bar

  , notifOutput      :: String -> IO ()
    -- ^ Rendering function for notifications, primarily intended for specifying
    -- an output pipe to for example dzen

  , notifPreProcess  :: Notif -> IO Notif
    -- ^ Function for manipulating notifications before any other formatting
    -- takes place

  , notifActions     :: M.Map String (Daemon -> Word32 -> IO ())
    -- ^ A map of action handlers called whenever a `ActionInvoked` signal is
    -- triggered with the given key.
  }

defaultNotifConf = NotifConf
  { notifDuration    = 5000000
  , notifFormat      = \notif -> summary notif ++ " " ++ body notif
  , notifFormatLog   = Nothing
  , notifPlay        = \_ -> return () -- no sounds by default
  , notifUpdateTimer = ClockSyncMinutes
  , notifStatusbar   = \log -> do
                               t <- getZonedTime
                               let c  = formatTime defaultTimeLocale "%a %b %d, %H:%M" t
                                   l  = M.size log
                                   l' = if l == 0 then "" else "[" ++ show l ++ "]"
                               return $ c ++ " " ++ l'
  , notifOutput      = putStrLn
  , notifPreProcess  = return . id
  , notifActions     = M.fromList []
  }


-- | internal state of the notification daemon
data State = State
  { currentLog    :: TVar NotifLog
  , currentId     :: TVar Word32
  , currentAction :: TVar (Maybe (Async ()))
  }

makeState :: IO State
makeState  = do
  l <- atomically . newTVar . M.fromList $ []
  i <- atomically . newTVar $ 0
  a <- atomically . newTVar $ Nothing
  return State
    { currentLog    = l
    , currentId     = i
    , currentAction = a
    }


-------------------------------------------------------------------------------
-- DBus methods
-------------------------------------------------------------------------------

getCapabilitiesInn = signature_ []
getCapabilitiesOut = signature_ [ TypeArray TypeString ]
getCapabilities :: MethodCall -> IO Reply
getCapabilities _ = do
  return . replyReturn $ [ body, sound, xvendor ]
  where
    body    = toVariant $ ("body"         :: String)
    sound   = toVariant $ ("sound"        :: String)
    xvendor = toVariant $ ("x-notifd-log" :: String)


notifyInn = signature_ [ TypeString, TypeWord32, TypeString, TypeString, TypeString, TypeArray TypeString, TypeDictionary TypeString TypeString, TypeInt32 ]
notifyOut = signature_ [ TypeWord32 ]
notify :: State -> NotifConf -> MethodCall -> IO Reply
notify state conf method = do

  let args          = methodCallBody method

  notif            <- notifPreProcess conf $ Notif
                        { notifId  = fromMaybe 0  . fromVariant $ args !! 1
                        , appName  = fromMaybe "" . fromVariant $ args !! 0
                        , appIcon  = fromMaybe "" . fromVariant $ args !! 2
                        , summary  = fromMaybe "" . fromVariant $ args !! 3
                        , body     = fromMaybe "" . fromVariant $ args !! 4
                        , actions  = fromMaybe [] . fromVariant $ args !! 5
                        , hints    = fromMaybe (M.fromList []) . fromVariant $ args !! 6
                        , duration = fromMaybe (-1) . fromVariant $ args !! 7
                        }

  let duration'     = fromIntegral . duration $ notif
      getHint k     = join . fmap fromVariant . M.lookup k $ hints notif

      -- suppress flags should be boolean, however, notify-send doesn't support
      -- that type and instead sends them as byte values, so we try both
      suppressLog   = or [ maybe False ((==) (1 :: Word8)) . getHint $ "suppress-log"
                         , fromMaybe False . getHint $ "suppress-log" ]
      suppressSound = or [ maybe False ((==) (1 :: Word8)) . getHint $ "suppress-sound"
                         , fromMaybe False . getHint $ "suppress-sound" ]

      soundFile     = getHint "sound-file" :: Maybe String

  newAction <- async $ do
    case compare duration' 0 of
      LT -> threadDelay $ notifDuration conf
      EQ -> forever getLine
      GT -> threadDelay $ duration' * 1000
    l <- atomically $ do
                      writeTVar (currentAction state) $ Nothing
                      readTVar . currentLog $ state
    notifOutput conf =<< notifStatusbar conf l

  -- update state
  (action, notif') <- atomically $ do
    a <- readTVar . currentAction $ state
    i <- case notifId notif of
           0 -> fmap (+1) . readTVar . currentId $ state
           i -> return i

    let notif'       = notif { notifId = i }
        logFormatted = fromMaybe (notifFormat conf) (notifFormatLog conf) notif'

    unless suppressLog $ modifyTVar' (currentLog state) (M.insert i logFormatted)
    writeTVar (currentAction state) $ Just newAction
    writeTVar (currentId state) $ i
    return (a, notif')

  forM_ action cancel
  unless suppressSound $ (forkIO . notifPlay conf $ soundFile) >> return ()
  notifOutput conf . notifFormat conf $ notif'
  return $ replyReturn [toVariant $ notifId notif']

-- we use the close method to clear or remove entries from the log
closeNotificationInn = signature_ [ TypeWord32 ]
closeNotificationOut = signature_ []
closeNotification :: Daemon -> State -> MethodCall -> IO Reply
closeNotification daemon state method = do
  let args    = methodCallBody method
      closeId = fromMaybe 0 . fromVariant $ args !! 0

  case closeId of
    0 -> atomically . writeTVar (currentLog state) $ M.fromList []
    i -> atomically . modifyTVar' (currentLog state) $ M.delete closeId

  ndUpdate daemon

  return $ replyReturn []


getServerInformationInn = signature_ []
getServerInformationOut = signature_ [ TypeString, TypeString, TypeString, TypeString ]
getServerInformation :: MethodCall -> IO Reply
getServerInformation _ = do
  return . replyReturn $ [ name, vendor, version, dono ]
  where
    name    = toVariant $ ("notifd" :: String)
    vendor  = toVariant $ ("notifd" :: String)
    version = toVariant $ ("0.0.0"  :: String)
    dono    = toVariant $ (""       :: String)


-------------------------------------------------------------------------------
-- DBus signals
-------------------------------------------------------------------------------

matchActionInvoked = matchAny
  { matchPath        = Just "/org/freedesktop/Notifications"
  , matchInterface   = Just "org.freedesktop.Notifications"
  , matchMember      = Just "ActionInvoked"
  , matchDestination = Just "org.freedesktop.Notifications"
  }
actionInvoked :: Daemon -> NotifConf -> Signal -> IO ()
actionInvoked daemon conf signal = do
  let maybes = do
               (notifId, actionKey) <- toPair . signalBody $ signal
               handler              <- M.lookup actionKey . notifActions $ conf
               return (handler, notifId)

  case maybes of
    Nothing                 -> return ()
    Just (handler, notifId) -> handler daemon notifId

  where
    toPair [x,y] = do
                   notifId   <- fromVariant x :: Maybe Word32
                   actionKey <- fromVariant y :: Maybe String
                   return (notifId, actionKey)
    toPair _     = Nothing


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

data Daemon = Daemon
  { ndGetLog   :: IO NotifLog
    -- ^ get the current log from the notification daemon

  , ndDelete   :: Word32 -> IO ()
    -- ^ remove a notification with the given id from the log

  , ndClearLog :: IO ()
    -- ^ clear the notification log

  , ndUpdate   :: IO ()
    -- ^ force a re-rendering of the status bar
  }

runNotifDaemon :: NotifConf -> IO (Maybe Daemon)
runNotifDaemon conf = do
  let name = busName_ "org.freedesktop.Notifications"
  state   <- makeState

  addr    <- getSessionAddress
  when (isNothing addr) . throwIO . clientError $ "bad DBUS_SESSION_BUS_ADDRESS"
  client  <- connectSession
  reply   <- requestName client name
               [ nameReplaceExisting
               , nameAllowReplacement
               , nameDoNotQueue
               ]

  let daemon = Daemon
                 { ndGetLog   = atomically . readTVar . currentLog $ state
                 , ndDelete   = atomically . modifyTVar (currentLog state) . M.delete
                 , ndClearLog = atomically . writeTVar (currentLog state) $ M.fromList []
                 , ndUpdate   = do
                                action <- atomically . readTVar . currentAction $ state
                                when (isNothing action) $ do
                                  l <- atomically . readTVar . currentLog $ state
                                  notifOutput conf =<< notifStatusbar conf l
                 }

  if reply /= NamePrimaryOwner
  then return Nothing
  else do
    export client "/org/freedesktop/Notifications"
      [ method "org.freedesktop.Notifications"
               "GetCapabilities"
               getCapabilitiesInn
               getCapabilitiesOut
               getCapabilities

      , method "org.freedesktop.Notifications"
               "Notify"
               notifyInn
               notifyOut
               (notify state conf)

      , method "org.freedesktop.Notifications"
               "CloseNotification"
               closeNotificationInn
               closeNotificationOut
               (closeNotification daemon state)

      , method "org.freedesktop.Notifications"
               "GetServerInformation"
               getServerInformationInn
               getServerInformationOut
               getServerInformation
      ]

    addMatch client matchActionInvoked . actionInvoked daemon $ conf

    ndUpdate daemon
    forkIO . forever $ do
      case notifUpdateTimer conf of
        ClockSyncMinutes -> do
                            d <- fmap (floor . (*) 1000000 . utctDayTime) getCurrentTime
                            threadDelay $ 60000000 - d `mod` 60000000
        ClockSyncSeconds -> do
                            d <- fmap (floor . (*) 1000000 . utctDayTime) getCurrentTime
                            threadDelay $ 1000000 - d `mod` 1000000
        Every n          -> threadDelay n
        Once             -> forever getLine -- block indefinitely

      ndUpdate daemon

    return $ Just daemon
