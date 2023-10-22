{-# LANGUAGE FlexibleContexts #-}

import XMonad hiding ( (|||) )


import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.Rescreen
import XMonad.Hooks.StatusBar ( statusBarPipe, withSB )
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WindowSwallowing

import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPanePersistent

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Shell

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.Hacks
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import Control.Monad ( foldM )

import Data.List ( nub )
import Data.Monoid ( All )

import System.Environment ( getEnv )
import XMonad.Layout.Fullscreen (fullscreenFull)

main :: IO ()
main = do
    n <- countScreens
    home <- getEnv "HOME"
    xmprocs <- mapM (\i -> statusBarPipe ("xmobar " ++ home ++ "/.config/xmobar/xmobar-"
            ++ show i ++ ".hs -x " ++ show i) $ pure myxmobarPP) [(0 :: Int)..n - 1]
    xmconfigs <- foldM (\x y -> return $ x <> y) mempty xmprocs
    xmonad $ javaHack $ withSB xmconfigs $ rescreenHook myRescreenCfg $ docks
           $ ewmhFullscreen $ ewmh $ myUrgencyHook def
        { manageHook         = myManageHook <+> namedScratchpadManageHook myScratchPads
        , layoutHook         = myLayoutHook
        , handleEventHook    = myEventHook
        , borderWidth        = 2
        , terminal           = myTerminal
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        , modMask            = mod4Mask
        , workspaces         = map show [(1 :: Int)..5]
        } `additionalKeysP` myKeys

myxmobarPP :: PP
myxmobarPP = def
    { ppCurrent         = xmobarColor "yellow" "" . wrap "[" "]"
    , ppHiddenNoWindows = xmobarColor "grey" "" . noScratchPad
    , ppHidden          = noScratchPad
    , ppTitle           = xmobarColor "green" "" . xmobarStrip
    , ppVisible         = wrap "(" ")"
    , ppUrgent          = xmobarColor "red" "yellow" . wrap "!" "!"
    }
    where noScratchPad ws = if ws == "NSP" then "" else ws

myXPromptConfig :: XPConfig
myXPromptConfig = def
    { font = "xft:Terminus:size=13"
    , bgColor = "black"
    , fgColor = "grey"
    , bgHLight = "brey"
    , fgHLight = "white"
 -- , borderColor = "black"
    , promptBorderWidth = 0
    , position = Top
    , alwaysHighlight = True
    , height = 20
    , maxComplRows = Just 5
    , historySize = 256
    , historyFilter = nub
    , promptKeymap = defaultXPKeymap
    , completionKey = (0, xK_Tab)
    , changeModeKey = xK_grave
    , defaultText = []
    , autoComplete = Nothing
    , showCompletionOnTab = False
    , searchPredicate = fuzzyMatch
    , defaultPrompter = id
    , sorter = fuzzySort
    }

myUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
myUrgencyHook = withUrgencyHookC
    BorderUrgencyHook { urgencyBorderColor = "red" }
    def { suppressWhen = Focused }

myRescreenCfg :: RescreenConfig
myRescreenCfg = def { afterRescreenHook = sequence_ [ safeSpawn "xmonad" ["--restart"]
                                                    , safeSpawn "nitrogen" ["--restore"]
                                                    , unsafeSpawn "$HOME/.config/xmonad/xrandr-full-color-range.sh"
                                                    ]
                    }

myTerminal :: String
myTerminal = "alacritty"

myKeys :: [(String, X ())]
myKeys = [ ( "<XF86AudioPlay>", safeSpawn "playerctl" ["play-pause"] )
         , ( "<XF86Favorites>", safeSpawn "playerctl" ["play-pause"] )
         , ( "<XF86AudioNext>", safeSpawn "playerctl" ["next"] )
         , ( "<XF86AudioPrev>", safeSpawn "playerctl" ["previous"] )
         , ( "<XF86AudioStop>", safeSpawn "playerctl" ["stop"] )
         , ( "<XF86AudioLowerVolume>", safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "-5%"] )
         , ( "<XF86AudioRaiseVolume>", safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "+5%"] )
         , ( "<XF86AudioMute>", safeSpawn "pactl" ["set-sink-mute", "@DEFAULT_SINK@", "toggle"] )
         , ( "<XF86AudioMicMute>", safeSpawn "pactl" ["set-source-mute", "@DEFAULT_SOURCE@", "toggle"] )
         , ( "M4-c", safeSpawn "pactl" ["set-source-mute", "@DEFAULT_SOURCE@", "toggle"] )
         , ( "S-M1-4", safeSpawn "pactl" ["set-source-mute", "@DEFAULT_SOURCE@", "toggle"] )
         , ( "<XF86MonBrightnessUp>", safeSpawn "xbacklight" ["-fps", "60", "+7.5"] )
         , ( "<XF86MonBrightnessDown>", safeSpawn "xbacklight" ["-fps", "60", "-7.5"] )
         , ( "M4-p", shellPrompt myXPromptConfig )
         , ( "M4-m", manPrompt myXPromptConfig )
         , ( "C-M1-l", safeSpawn "light-locker-command" ["-l"] )
         , ( "<F12>", scratchPad )
         , ( "<XF86Display>", safeSpawnProg "arandr" )
         , ( "<XF86Tools>", safeSpawnProg "blueman-manager" )
         , ( "<Print>", safeSpawn "gnome-screenshot" ["--interactive"] )
         , ( "M4-b", sendMessage ToggleStruts )
         , ( "M4-<Backspace>", focusUrgent )
         , ( "M4-a", sendMessage MirrorShrink )
         , ( "M4-z", sendMessage MirrorExpand )
         , ( "S-M4-q", unsafeSpawn "loginctl terminate-session $XDG_SESSION_ID" )
         , ( "C-<Space>", safeSpawn "dunstctl" ["close"] )
         , ( "C-S-<Space>", safeSpawn "dunstctl" ["close-all"] )
         , ( "C-`", safeSpawn "dunstctl" ["history-pop"] )
         , ( "C-S-,", safeSpawn "dunstctl" ["action"] )
         , ( "C-S-.", safeSpawn "dunstctl" ["context"] )
         ]
    where scratchPad = namedScratchpadAction myScratchPads "terminal"

myManageHook :: ManageHook
myManageHook = do
    n <- countScreens :: Query Int
    composeAll
        [ isDialog --> doCenterFloat
        , isFullscreen --> doFullFloat
        , className =? "alacritty" <&&> title =? "neomutt" --> shift n
        , className =? "firefox" <&&> title =? "Picture-in-Picture" --> doFloat
        , (className =? "firefox" <||> className =? "thunderbird") <&&> role =? "Popup" --> hasBorder False <+> doFloat
        , className =? "gcr-prompter" <||> className =? "Gcr-prompter" --> doCenterFloat
        , className =? "discord" --> shift n
        , className =? "hexchat" <||> className =? "Hexchat" --> shift n
        , className =? "telegram-desktop" <||> className =? "TelegramDesktop" <&&> title =? "Telegram" --> shift n
          -- floating videos
        , className =? "telegram-desktop" <||> className =? "TelegramDesktop" <&&> isUtility --> hasBorder False <+> doCenterFloat
        ]
    where role = stringProperty "WM_WINDOW_ROLE"
          shift n = doShift $ if n > 1 then "1" else "2"

isUtility :: Query Bool
isUtility = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY"

myLayoutHook = lessBorders Screen $ refocusLastLayoutHook $ avoidStruts $
               trackFloating $
                        tabbed shrinkText myTabbedCfg
                    ||| ResizableTall 1 (3/100) (1/2) []
                    ||| TwoPanePersistent Nothing (3/100) (1/2)
    where myTabbedCfg = def { activeColor         = "black"
                            , inactiveColor       = "black"
                            , urgentColor         = "yellow"
                            , activeBorderColor   = "#cd8b00"
                            , inactiveBorderColor = "#cccccc"
                            , urgentBorderColor   = "red"
                         -- , activeBorderWidth
                            , inactiveBorderWidth   = 0
                         -- , urgentBorderWidth
                            , activeTextColor     = "grey"
                            , inactiveTextColor   = "grey"
                            , urgentTextColor     = "red"
                            , fontName            = "xft:Terminus:size=12"
                         -- , decoWidth
                         -- , decoHeight
                         -- , windowTitleAddons
                         -- , windowTitleIcons
                            }

myEventHook = refocusLastWhen isFloat
              <+> dynamicPropertyChange "WM_CLASS" myDynHook

myDynHook :: ManageHook
myDynHook = do
    n <- countScreens
    let desktop = if (n :: Int) > 1 then "1" else "2"
    composeAll
        [ className =? "spotify" <||> className =? "Spotify" --> doShift desktop
        ]

{-
mySwallowEventHook :: Event -> X All
mySwallowEventHook = swallowEventHook
    (className =? "Alacritty" <||> className =? "tmux" <||> className =? "Termite") $ return True
-}

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm ]
    where
        spawnTerm  = myTerminal ++ " --title Scratchpad"
        findTerm   = title =? "Scratchpad" <&&> className =? "Alacritty"
        manageTerm = customFloating $ W.RationalRect l t w h
            where
                h = 0.35
                w = 1
                t = 1 - h - 0.016
                l = 1 - w
