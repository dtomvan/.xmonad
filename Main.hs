import qualified Data.Map as M
import XMonad
import XMonad.ManageHook

-- Actions
import XMonad.Actions.Submap

-- Utils
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
import XMonad.Util.Types

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- Layouts
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed
import XMonad.Layout.ThreeColumns

modm :: KeyMask
modm = mod4Mask

-- Main function
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (clickablePP myXmobarPP)) defToggleStrutsKey
     $ myConfig
     `additionalKeysP`
     [
         -- BSP
         ("M-r", modalmap $ mkKeymap myConfig [
          ("h", sendMessage $ ExpandTowards L),
          ("j", sendMessage $ ExpandTowards D),
          ("k", sendMessage $ ExpandTowards U),
          ("l", sendMessage $ ExpandTowards R),
          ("C-h", sendMessage $ ShrinkFrom L),
          ("C-j", sendMessage $ ShrinkFrom D),
          ("C-k", sendMessage $ ShrinkFrom U),
          ("C-l", sendMessage $ ShrinkFrom R)
         ])
         , ("M-s",            sendMessage $ Swap)
         , ("M-M1-s",         sendMessage $ Rotate)
         , ("M-S-C-j",        sendMessage $ SplitShift Prev)
         , ("M-S-C-k",        sendMessage $ SplitShift Next)
     ]
     -- `additionalKeysP`
     --    [
     --        ("<XF86AudioLowerVolume>", spawn "amixer set Master 1%-"),
     --        ("<XF86AudioRaiseVolume>", spawn "amixer set Master 1%+")
     --    ]

-- Modal mappings
modalmap :: M.Map (KeyMask, KeySym) (X ()) -> X ()
modalmap s = submap $ M.map (>> modalmap s) s

-- Configuration
myConfig = def {
    modMask = mod4Mask,
    terminal = "st -e fish",
    layoutHook = myLayout,
    startupHook = do
        spawnOnce "xsetroot -cursor_name left_ptr"
        spawnOnce "feh --bg-fill --no-fehbg ~/.xmonad/bg.png"
        spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x5f5f5f --height 16 &"
}


-- Simple managehook, lets dialog boxes float.
myManageHook :: ManageHook
myManageHook = composeAll
    [isDialog --> doFloat]

-- Layout definition.
myLayout = bsp ||| tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    bsp = renamed [Replace "|="] $ emptyBSP
    threeCol
        = renamed [Replace "|||"]
        $ magnifiercz' 1.3
        $ ThreeColMid nmaster delta ratio
    tiled   = renamed [Replace "[]="] $ Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = xmobarPP {
    ppTitle = shorten 100,
    ppHiddenNoWindows = xmobarColor "#333" "",
    ppVisibleNoWindows = Just (xmobarColor "#333" "")
}
