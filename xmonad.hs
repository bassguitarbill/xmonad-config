import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.CycleWS

main :: IO()

--main = xmonad . ewmh =<< withEasySB (statusBarProp "xmobar" (pure def)) defToggleStrutsKey $ myConfig
main = xmonad . ewmh =<< xmobar myConfig
--
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    --, ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow
    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30
    
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myConfig = def
    { -- modMask    = mod4Mask, can't do this on windows woooooo
      terminal = "st",
      layoutHook = myLayout,
      normalBorderColor = "white",
      focusedBorderColor = "fuchsia"
    }
  `additionalKeysP`
    [ ("M-S-=", unGrab *> spawn "scrot -s"        )
    , ("M-]"  , spawn "firefox"                   )
    , ("M-<L>", prevWS                            )
    , ("M-<R>", nextWS                            )
    ]

myLayout = tiled ||| Mirror tiled ||| Full 
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
