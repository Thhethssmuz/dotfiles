{-# LANGUAGE DeriveDataTypeable #-}

import XMonad hiding ((|||))

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed

import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksEventHook, ToggleStruts(..))
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (isInProperty, isFullscreen, doFullFloat)
import XMonad.Util.WorkspaceCompare (getSortByXineramaPhysicalRule)

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Util.Run (spawnPipe, safeSpawn, runProcessWithInput)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Util.WindowProperties (getProp32)

import Prompts
import XMonad.Prompt

import DzenMenu
import Notifd
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Char (isSpace)
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Applicative ((<$>))

import System.Directory (getHomeDirectory)
import System.IO (hPutStr, hPutStrLn, hSetEncoding, utf8)
import System.Exit

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main = do
  home   <- fmap (flip (++) "/") getHomeDirectory
  left   <- spawnPipe $ home ++ ".xmonad/scripts/dzen-bar-left.sh"
  middle <- spawnPipe $ home ++ ".xmonad/scripts/dzen-bar-middle.sh"
  hSetEncoding middle utf8

  notifd <- runNotifDaemon $ myNotifConf home middle
  runDzenMenus . myDzenMenus home $ notifd
  xmonad
    . withUrgencyHookC CMDNotify myUrgencyConf
    . ewmh
    $ def
    { borderWidth        = 1
    , workspaces         = myWorkspaces
    , layoutHook         = myLayout
    , terminal           = "urxvt"
    , normalBorderColor  = background
    , focusedBorderColor = foreground
    , modMask            = mod1Mask
    , keys               = myKeys home
    , logHook            = myLogHook left
    -- , startupHook        = myStartupHook
    -- , mouseBindings      = mouseBindings
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook
    , focusFollowsMouse  = True
    -- , clickJustFocuses   = clickJustFocuses
    }

-------------------------------------------------------------------------------
-- Workspaces
-------------------------------------------------------------------------------

myWorkspaces = clickable $ map (:[]) set
  where
    set       = "123456789"
    clickable = map ca . zip ks
    ca (k,w)  = "^ca(1, xdotool key alt+" ++ k ++ ")" ++ w ++ "^ca()"
    ks        = [ "aring", "comma", "period", "a", "o", "e", "ae", "q", "j" ]

-------------------------------------------------------------------------------
-- Layouts
-------------------------------------------------------------------------------

myLayoutIDs = [ "S", "T", "F", "G" ]

myLayout = tiledSpace ||| tiled ||| fullScreen ||| grid
  where

    tiledSpace = renamed [Replace "S"]
               . avoidStruts
               . spacing 60
               $ ResizableTall nmaster delta ratio []

    tiled      = renamed [Replace "T"]
               . avoidStruts
               . spacing 5
               $ ResizableTall nmaster delta ratio []

    fullScreen = renamed [Replace "F"]
               . noBorders
               $ fullscreenFull Full

    grid       = renamed [Replace "G"]
               . avoidStruts
               . spacing 5
               $ GridRatio (16/9)

    nmaster    = 1
    delta      = 5/100
    ratio      = toRational (2/(1 + sqrt 5 :: Double))


-- Layout state that stores previous layout, allows to toggle fullscreen :D
data LayoutState = LayoutState
  { layoutMap :: M.Map String (String, String)
  } deriving (Typeable, Show)

instance ExtensionClass LayoutState where
  initialValue = LayoutState $ M.fromList []

-- jump to layout i, unless we are already on layout i then go back to previous
layout i = do
  ws    <- gets (W.currentTag . windowset)
  m     <- fmap layoutMap XS.get

  let h       = head myLayoutIDs
      (c, p ) = fromMaybe (h,h) . M.lookup ws $ m
      (c',p') = if i == c then (p,c) else (i,c)

  XS.put . LayoutState . M.insert ws (c',p') $ m
  sendMessage . JumpToLayout $ c'

-- next layout
cycleLayouts = do
  ws    <- gets (W.currentTag . windowset)
  m     <- fmap layoutMap XS.get

  let h     = head myLayoutIDs
      (c,_) = fromMaybe (h,h) . M.lookup ws $ m
      c'    = (!!) (cycle myLayoutIDs)
            . (+1) . fromJust . findIndex (== c) $ myLayoutIDs

  XS.put . LayoutState . M.insert ws (c',c) $ m
  sendMessage NextLayout

resetLayoutState l = do
  ws    <- gets (W.currentTag . windowset)
  m     <- fmap layoutMap XS.get
  XS.put . LayoutState . M.delete ws $ m
  setLayout l

-------------------------------------------------------------------------------
-- Urgency hook
-------------------------------------------------------------------------------

data CMDNotify = CMDNotify deriving (Show, Read)

instance UrgencyHook CMDNotify where
  urgencyHook _ w = do
    name <- fmap show . getName $ w
    ws   <- fmap (fromMaybe "?" . W.findTag w) . gets $ windowset

    safeSpawn "notify-send"
      [ name
      , dzenStrip ws
      , "-h"
      , "byte:suppress-log:1"
      , "-h"
      , "string:sound-file:/usr/share/sounds/freedesktop/stereo/window-attention.oga"
      ]

    withDisplay $ \d -> io $ fromJust <$> initColor d "red" >>= setWindowBorder d w

myUrgencyConf  = urgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-------------------------------------------------------------------------------
-- Notifications (status bar middle)
-------------------------------------------------------------------------------

myNotifConf home statusBar = defaultNotifConf
  { notifFormat     = toggleMenu "Notif" . myNotifFormat
  , notifFormatLog  = Just $ \n -> (++) (myNotifFormat n)
                           . fg color1
                           . (++) "^p(_RIGHT)^p(-35)"
                           . closeNotif (notifId n) $ "x"
  , notifPlay       = \x -> case x of
                        Just fp -> spawn $ "paplay " ++ fp
                        _       -> spawn "paplay /usr/share/sounds/freedesktop/stereo/bell.oga"

  , notifStatusbar  = \l -> do
                        t <- getZonedTime
                        let c = formatTime defaultTimeLocale "%a %b %d, %H:%M" t
                            s = if M.size l == 0
                                then ""
                                else dzenColor background color1 . pad . show . M.size $ l
                        return $ toggleMenu "Calendar" c ++ " " ++ toggleMenu "Notif" s

  , notifOutput     = hPutStrLn statusBar

  , notifPreProcess = \n -> do
                        a <- runProcessWithInput (home ++ ".xmonad/scripts/notif-icon.sh") [appIcon n] ""
                        return $ n { appIcon = a }
  }
  where
    limit _ []                     = []
    limit l (x:xs) | length x >  l = [take l x]
                   | otherwise     = x : limit (l - length x) xs

    myNotifFormat n = intercalate " "
                    . (\[i,s,b] -> [i, s, fg color8 b])
                    . (++) [appIcon n]
                    . limit ( if null . appIcon $ n then 50 else 47 )
                    $ [summary n, takeWhile (/= '\n') $ body n]

    toggleMenu n    = ca 1 $ home ++ ".xmonad/scripts/dbus.sh menu Toggle " ++ n
    closeNotif n    = ca 1 $ home ++ ".xmonad/scripts/dbus.sh close-notif " ++ show n

-------------------------------------------------------------------------------
-- Dzen menus
-------------------------------------------------------------------------------

notifLogMenu home notifd = dzenMenu
  { menuName   = "Notif"
  , menuScript = home ++ ".xmonad/scripts/dzen-menu-notif-log.sh"
  , menuSlave  = \ext -> do
                         l <- ndGetLog notifd
                         return . concatMap (wrap "^p(+20)- " "\n") . M.elems $ l
  }

myDzenMenus home notifd =
  [ dzenMenu { menuName   = "Calendar"
             , menuScript = home ++ ".xmonad/scripts/dzen-menu-calendar.sh"
             }
  , dzenMenu { menuName   = "Updates"
             , menuScript = home ++ ".xmonad/scripts/dzen-menu-updates.sh"
             }
  , dzenMenu { menuName   = "Dropbox"
             , menuScript = home ++ ".xmonad/scripts/dzen-menu-dropbox.sh"
             }
  ] ++ maybeToList (notifLogMenu home <$> notifd)

-------------------------------------------------------------------------------
-- Manage hooks
-------------------------------------------------------------------------------

myManageHook = (composeAll
  [ resource  =? "dzen2"       --> doIgnore
  , resource  =? "feh"         --> doIgnore
  , resource  =? "guake"       --> doFloat
  , className =? "stalonetray" --> doIgnore
  --, isSticky                   --> doIgnore
  , manageDocks
  ]) <+> manageHook def
  --where
  --  isSticky = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_STICKY"

-------------------------------------------------------------------------------
-- Handle event hooks
-------------------------------------------------------------------------------

myHandleEventHook = refreshOnFullscreen <+> fullscreenEventHook <+> docksEventHook

-- hack for fullscreening chromium inside a tiled window
refreshOnFullscreen :: Event -> X All
refreshOnFullscreen (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc  <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate  <- fromMaybe [] `fmap` getProp32 wmstate win

  when (typ == wmstate && fromIntegral fullsc `elem` dats) $ do
    when (action == 1 || (action == 2 && not (fromIntegral fullsc `elem` wstate))) $ do
      sendMessage Shrink
      sendMessage Expand

  return $ All True

refreshOnFullscreen _ = return $ All True

-------------------------------------------------------------------------------
-- Status bar (left)
-------------------------------------------------------------------------------

myLogHook h = dynamicLogWithPP $ def
  { ppCurrent   = fg color12 . pad
  , ppVisible   = fg color4 . pad
  , ppUrgent    = fg color1 . pad
  , ppHidden    = pad
  , ppWsSep     = ""
  , ppSep       = "   "
  , ppSort      = getSortByXineramaPhysicalRule
  , ppOrder     = \(ws:_:t:_) -> [ ' ':ws, t ]
  , ppOutput    = hPutStrLn h
  }

-------------------------------------------------------------------------------
-- Prompt
-------------------------------------------------------------------------------

myXPConf = def
  { font                = "xft:Ubuntu Mono:size=12:Bold"
  , promptBorderWidth   = 0
  , fgColor             = color15
  , bgColor             = background
  , fgHLight            = color15
  , bgHLight            = background
  , height              = 22
  , position            = Top
  , historySize         = 2048
  , changeModeKey       = xK_F24
  , promptKeymap        = M.fromList
    [ ((controlMask,            xK_v        ), pasteString)

    , ((0,                      xK_BackSpace), deleteString Prev)
    , ((mod1Mask,               xK_BackSpace), killWord' isSpace Prev)
    , ((0,                      xK_Delete   ), deleteString Next)
    , ((mod1Mask,               xK_Delete   ), killWord' isSpace Next)

    , ((0,                      xK_Left     ), moveCursor Prev)
    , ((mod1Mask,               xK_Left     ), moveWord' isSpace Prev)
    , ((0,                      xK_Right    ), moveCursor Next)
    , ((mod1Mask,               xK_Right    ), moveWord' isSpace Next)

    , ((0,                      xK_Home     ), startOfLine)
    , ((controlMask,            xK_a        ), startOfLine)
    , ((0,                      xK_End      ), endOfLine)
    , ((controlMask,            xK_e        ), endOfLine)

    , ((0,                      xK_Escape   ), quit)

    , ((mod1Mask,               xK_space    ), spawn "xdotool key F24")
    , ((0,                      xK_Return   ), setSuccess True >> setDone True)
    ]
  }

-------------------------------------------------------------------------------
-- Key Bindings
-------------------------------------------------------------------------------

myKeys home conf@(XConfig { modMask = modMask }) = M.fromList $
  [ ((modMask .|. shiftMask,    xK_Return ), spawn $ terminal conf)
  , ((modMask,                  xK_F2     ), spawn "gmrun")

  , ((modMask,                  xK_r      ), prompt  myXPConf [bash, pass])
  , ((modMask .|. shiftMask,    xK_r      ), prompt  myXPConf [calc])
  , ((modMask .|. shiftMask,    xK_t      ), prompt' myXPConf [defi, enno, noen])

  , ((modMask .|. shiftMask,    xK_c      ), kill)

  , ((modMask .|. shiftMask,    xK_space  ), resetLayoutState $ XMonad.layoutHook conf)
  , ((modMask,                  xK_space  ), cycleLayouts)
  , ((modMask,                  xK_f      ), layout "F")
  , ((modMask,                  xK_g      ), layout "G")

  , ((mod1Mask,                 xK_Tab    ), windows W.focusDown)
  , ((mod1Mask .|. shiftMask,   xK_Tab    ), windows W.focusUp)
  , ((modMask,                  xK_m      ), windows W.focusMaster)

  , ((modMask,                  xK_Return ), windows W.swapMaster)

  , ((modMask,                  xK_minus  ), sendMessage Shrink)
  , ((modMask,                  xK_plus   ), sendMessage Expand)
  , ((modMask,                  xK_t      ), withFocused $ windows . W.sink)
  -- , ((modMask,                   ), sendMessage $ IncMasterN 1)
  -- , ((modMask,                   ), sendMessage $ IncMasterN (-1))


  -- set focus to what ever workspace is visible on screen n
  , ((modMask,                  xK_1      ), screenWorkspace 2 >>= flip whenJust (windows . W.view))
  , ((modMask,                  xK_2      ), screenWorkspace 0 >>= flip whenJust (windows . W.view))
  , ((modMask,                  xK_3      ), screenWorkspace 1 >>= flip whenJust (windows . W.view))

  -- move window to what ever workspace is visible on screen n
  , ((modMask .|. shiftMask,    xK_1      ), screenWorkspace 2 >>= flip whenJust (windows . W.shift))
  , ((modMask .|. shiftMask,    xK_2      ), screenWorkspace 0 >>= flip whenJust (windows . W.shift))
  , ((modMask .|. shiftMask,    xK_3      ), screenWorkspace 1 >>= flip whenJust (windows . W.shift))

  -- set focus to workspace n
  , ((modMask,                   xK_aring ), windows . W.greedyView $ workspaces conf !! 0)
  , ((modMask,                   xK_comma ), windows . W.greedyView $ workspaces conf !! 1)
  , ((modMask,                   xK_period), windows . W.greedyView $ workspaces conf !! 2)
  , ((modMask,                   xK_a     ), windows . W.greedyView $ workspaces conf !! 3)
  , ((modMask,                   xK_o     ), windows . W.greedyView $ workspaces conf !! 4)
  , ((modMask,                   xK_e     ), windows . W.greedyView $ workspaces conf !! 5)
  , ((modMask,                   xK_ae    ), windows . W.greedyView $ workspaces conf !! 6)
  , ((modMask,                   xK_q     ), windows . W.greedyView $ workspaces conf !! 7)
  , ((modMask,                   xK_j     ), windows . W.greedyView $ workspaces conf !! 8)

  -- move window to workspace n
  , ((modMask .|. shiftMask,     xK_aring ), windows . W.shift      $ workspaces conf !! 0)
  , ((modMask .|. shiftMask,     xK_comma ), windows . W.shift      $ workspaces conf !! 1)
  , ((modMask .|. shiftMask,     xK_period), windows . W.shift      $ workspaces conf !! 2)
  , ((modMask .|. shiftMask,     xK_a     ), windows . W.shift      $ workspaces conf !! 3)
  , ((modMask .|. shiftMask,     xK_o     ), windows . W.shift      $ workspaces conf !! 4)
  , ((modMask .|. shiftMask,     xK_e     ), windows . W.shift      $ workspaces conf !! 5)
  , ((modMask .|. shiftMask,     xK_ae    ), windows . W.shift      $ workspaces conf !! 6)
  , ((modMask .|. shiftMask,     xK_q     ), windows . W.shift      $ workspaces conf !! 7)
  , ((modMask .|. shiftMask,     xK_j     ), windows . W.shift      $ workspaces conf !! 8)


  -- guake
  , ((0,                         xK_bar   ), spawn "guake -t")

  -- screen lock
  , ((mod1Mask .|. controlMask,  xK_l     ), spawn $ home ++ ".xmonad/scripts/system.sh --lock")
  , ((modMask .|. shiftMask,     xK_F12   ), spawn (home ++ ".xmonad/scripts/system.sh --logout") >> io (exitWith ExitSuccess))
  , ((modMask,                   xK_F5    ), spawn $ "ghc -threaded -i" ++ home ++ ".xmonad/lib ~/.xmonad/xmonad.hs -o ~/.xmonad/xmonad-x86_64-linux && xmonad --restart")
  , ((modMask,                   xK_F6    ), sendMessage ToggleStruts)

  -- print screen
  , ((0,                         xK_Print ), spawn $ home ++ ".xmonad/scripts/screenshot.sh")
  , ((shiftMask,                 xK_Print ), spawn $ home ++ ".xmonad/scripts/screenshot.sh -u")

  -- volume control
  , ((0,                        0x1008FF12), spawn $ home ++ ".xmonad/scripts/volume.sh -m")
  , ((0,                        0x1008FF11), spawn $ home ++ ".xmonad/scripts/volume.sh -d")
  , ((0,                        0x1008FF13), spawn $ home ++ ".xmonad/scripts/volume.sh -i")

  -- media buttons
  , ((0,                        0x1008ff14), spawn "mpc toggle")
  , ((0,                        0x1008ff16), spawn "mpc prev")
  , ((0,                        0x1008ff17), spawn "mpc next")

  -- espeak
  , ((0,                        xK_F9     ), spawn "xsel | espeak -s 240")
  , ((shiftMask,                xK_F9     ), spawn "xsel | espeak -v no -s 240")
  , ((0,                        xK_F10    ), spawn "pkill -9 espeak")

  ]

-------------------------------------------------------------------------------
-- Colours and dzen helpers
-------------------------------------------------------------------------------

foreground = "#FFFFFF"
background = "#000000"
color0     = "#2E3436"
color1     = "#CC0000"
color2     = "#4E9A06"
color3     = "#C4A000"
color4     = "#3465A4"
color5     = "#75507B"
color6     = "#06989A"
color7     = "#D3D7CF"
color8     = "#555753"
color9     = "#EF2929"
color10    = "#8AE234"
color11    = "#FCE94F"
color12    = "#729FCF"
color13    = "#AD7FA8"
color14    = "#32E2E2"
color15    = "#EEEEEC"

fg :: String -> String -> String
fg color text = "^fg(" ++ color ++ ")" ++ text ++ "^fg()"

bg :: String -> String -> String
bg color text = "^bg(" ++ color ++ ")" ++ text ++ "^bg()"

ca :: Int -> String -> String -> String
ca btn cmd text = "^ca(" ++ show btn ++ ", " ++ cmd ++ ")" ++ text ++ "^ca()"
