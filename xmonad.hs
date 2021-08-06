import XMonad
import XMonad.ManageHook

-- Utils
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

-- Layouts
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Renamed

-- Main function
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig
     `additionalKeysP`
        [
            ("<XF86AudioLowerVolume>", spawn "amixer set Master 1%-"),
            ("<XF86AudioRaiseVolume>", spawn "amixer set Master 1%+")
        ]

-- Configuration
myConfig = def {
    modMask = mod4Mask,
    terminal = "st -e fish",
    layoutHook = myLayout,
    startupHook = do
        spawnOnce "xsetroot -cursor_name left_ptr"
        spawnOnce "feh --bg-fill --no-fehbg ~/.xmonad/bg.png"
        spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x5f5f5f --height 16 &"
        spawnOnce "volumeicon &"
        spawnOnce "picom --backend glx --xrender-sync-fence &"
}

-- Completely stolen from the new xmonad docs, looks good
myXmobarPP :: PP
myXmobarPP = def {
    ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
} where 
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 50

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    -- TODO: change these
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

-- Simple managehook, lets dialog boxes float.
myManageHook :: ManageHook
myManageHook = composeAll
    [isDialog --> doFloat]

-- Layout definition.
myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol
        = renamed [Replace "|||"]
        $ magnifiercz' 1.3
        $ ThreeColMid nmaster delta ratio
    tiled   = renamed [Replace "[]="] $ Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
