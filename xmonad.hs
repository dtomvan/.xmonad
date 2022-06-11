import qualified Data.Map as M
import XMonad

-- Actions
import XMonad.Actions.Submap
import XMonad.Actions.WindowNavigation

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks

-- Layouts
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

-- Utils
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Types
import XMonad.Util.Ungrab

import qualified XMonad.StackSet as W

import XMonad.Config.Desktop
import XMonad.ManageHook

modm :: KeyMask
modm = mod4Mask

-- Main function
main :: IO ()
main = do
  safeSpawn "mkfifo" ["/tmp/.xmonad-workspace-log"]
  spawn "$HOME/.config/polybar/launch.sh"
  -- Up, Left, Down, Right
  cfg <- withWindowNavigation (xK_k, xK_h, xK_j, xK_l) myConfig
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    $ cfg

-- Modal mappings
modalmap :: M.Map (KeyMask, KeySym) (X ()) -> X ()
modalmap s = submap $ M.map (>> modalmap s) s

-- Configuration
myConfig = additionalKeysP (removeKeysP xConf remKeys) myKeys

myKeys =
  [ -- BSP
    ( "M-r",
      modalmap $
        mkKeymap
          myConfig
          [ ("h", sendMessage $ ExpandTowards L),
            ("j", sendMessage $ ExpandTowards D),
            ("k", sendMessage $ ExpandTowards U),
            ("l", sendMessage $ ExpandTowards R),
            ("C-h", sendMessage $ ShrinkFrom L),
            ("C-j", sendMessage $ ShrinkFrom D),
            ("C-k", sendMessage $ ShrinkFrom U),
            ("C-l", sendMessage $ ShrinkFrom R)
          ]
    ),
    ("M-s", sendMessage $ Swap),
    ("M-M1-s", sendMessage $ Rotate),
    ("M-S-C-j", sendMessage $ SplitShift Prev),
    ("M-S-C-k", sendMessage $ SplitShift Next),
    ("M-d", spawn "dmenu_drun")
  ]

remKeys = ["M-h", "M-j", "M-k", "M-l"]

xConf =
  desktopConfig
    { modMask = mod4Mask,
      terminal = "st",
      layoutHook = myLayout,
      logHook = eventLogHook,
      startupHook = do
        spawnOnce "xsetroot -cursor_name left_ptr",
      normalBorderColor =  "#333",
      focusedBorderColor =  "#578fd6",
      borderWidth = 2
    }

-- Simple managehook, lets dialog boxes float.
myManageHook :: ManageHook
myManageHook =
  composeAll
    [isDialog --> doFloat,
     className =? "activate-linux" --> doIgnore,
     className =? "i3lock" --> doIgnore,
     className =? "Mumble" --> doFloat,
     className =? "powermenu" --> doFloat,
     className =? "discord" --> doShift "8"]

-- Layout definition.
myLayout = spacing 6 $ avoidStruts (bsp ||| tiled ||| Mirror tiled ||| Full ||| threeCol)
  where
    bsp = renamed [Replace "|="] $ emptyBSP
    threeCol =
      renamed [Replace "|||"] $
        magnifiercz' 1.3 $
          ThreeColMid nmaster delta ratio
    tiled = renamed [Replace "[]="] $ Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

-- Polybar integration
eventLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))
