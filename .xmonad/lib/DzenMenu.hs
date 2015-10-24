module DzenMenu
  ( DzenMenu(..)
  , runDzenMenus
  ) where

import Control.Monad
import Control.Concurrent.STM
import Control.Exception.Base (throwIO)

import Data.List
import Data.Maybe
import qualified Data.Map as M

import DBus
import DBus.Client

import GHC.IO.Handle (Handle)
import System.IO
import XMonad.Util.Run


safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing


data DzenMenu = DzenMenu
  { menuName   :: String
  , menuTitle  :: (String -> IO String) -> IO String
  , menuSlave  :: (String -> IO String) -> IO String
  , menuScript :: String
  }


data MenuState = MenuState
  { script :: String
  , title  :: (String -> IO String) -> IO String
  , slave  :: (String -> IO String) -> IO String
  , handle :: TVar (Maybe Handle)
  }

type State = [(String, MenuState)]

makeState :: [DzenMenu] -> IO State
makeState xs =
  forM xs $ \menu -> do
    h <- atomically . newTVar $ Nothing
    let menuState = MenuState
                      { script = menuScript menu
                      , title  = menuTitle  menu
                      , slave  = menuSlave  menu
                      , handle = h
                      }
    return $ (menuName menu, menuState)


toggleMenu :: String -> MenuState -> IO ()
toggleMenu name menu = do
  h <- atomically . readTVar . handle $ menu
  if isNothing h then openMenu  name menu
                 else closeMenu name menu

openMenu :: String -> MenuState -> IO ()
openMenu name menu = do
  t <- title menu $ runProcessWithInput (script menu) ["title"]
  s <- slave menu $ runProcessWithInput (script menu) ["slave"]
  h <- spawnPipe . intercalate " " $
         [ "/home/thhethssmuz/.xmonad/lib/DzenMenu.sh"
         , name
         , script menu
         , "main"
         , "-l"
         , show . length . lines $ s
         ]
  hPutStr h t
  hPutStr h s
  atomically . writeTVar (handle menu) . Just $ h

closeMenu :: String -> MenuState -> IO ()
closeMenu name menu = do
  h <- atomically $ do
                    h <- readTVar . handle $ menu
                    writeTVar (handle menu) Nothing
                    return h
  when (isJust h) . hClose . fromJust $ h

refreshMenu :: String -> MenuState -> IO ()
refreshMenu name menu = do
  h <- atomically . readTVar . handle $ menu

  unless (isNothing h) $ do
    t <- title menu $ runProcessWithInput (script menu) ["title"]
    s <- slave menu $ runProcessWithInput (script menu) ["slave"]
    hPutStr (fromJust h) . (++) "^tw()"   $ t
    hPutStr (fromJust h) . (++) "^cs()\n" $ s


signalHandler :: State -> Signal -> IO ()
signalHandler state signal = do
  let member = formatMemberName . signalMember $ signal
      m      = do
               arg  <- safeHead . signalBody $ signal
               name <- fromVariant arg
               menu <- lookup name state
               return (member, name, menu)

  case m of
    Just ("Toggle",  n, m) -> toggleMenu  n m
    Just ("Open",    n, m) -> openMenu    n m
    Just ("Close",   n, m) -> closeMenu   n m
    Just ("Refresh", n, m) -> refreshMenu n m
    _                      -> return ()


runDzenMenus :: [DzenMenu] -> IO ()
runDzenMenus xs = do
  let name = busName_ $ "org.DzenMenu.Server"

  state   <- makeState xs
  addr    <- getSessionAddress
  when (isNothing addr) . throwIO . clientError $ "bad DBUS_SESSION_BUS_ADDRESS"
  client  <- connectSession
  reply   <- requestName client name
               [ nameReplaceExisting
               , nameAllowReplacement
               , nameDoNotQueue
               ]

  let match = matchAny
                { matchPath        = Just . objectPath_ $ "/org/DzenMenu/Server"
                , matchInterface   = Just . interfaceName_ $ "org.DzenMenu.Server"
                , matchDestination = Just name
                }

  addMatch client match . signalHandler $ state

  return ()
