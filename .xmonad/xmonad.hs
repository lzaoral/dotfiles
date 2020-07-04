{-# LANGUAGE FlexibleContexts #-}

import XMonad

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.FuzzyMatch

import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.Run ( spawnPipe )
import XMonad.Util.NamedScratchpad

import Data.List ( nub )
import System.IO ( hPutStrLn )

main :: IO ()
main = do
    n <- countScreens
    xmprocs <- mapM (\i -> spawnPipe $ "xmobar ~/.xmonad/xmobar-" ++ show i ++ ".hs -x " ++ show i) ([0..n-1] :: [Int])
    xmonad $ myUrgencyHook $ ewmh def
        { manageHook         = myManageHook <+> manageDocks <+> namedScratchpadManageHook myScratchPads <+> manageHook def
        , layoutHook         = avoidStruts $ smartBorders $ onWorkspace (getMainWorkspace n) (noBorders Full) $ tall ||| Mirror tall ||| noBorders Full
        , handleEventHook    = handleEventHook def <+> docksEventHook <+> fullscreenEventHook <+> dynamicPropertyChange "WM_CLASS" myDynHook
        , logHook            = logHook def <+> mapM_ (\handle -> dynamicLogWithPP $ xmobarPP
                { ppOutput          = hPutStrLn handle
                , ppCurrent         = xmobarColor "yellow" "" . wrap "[" "]"
                , ppHiddenNoWindows = xmobarColor "grey" "" . noScratchPad
                , ppHidden          = noScratchPad
                , ppTitle           = xmobarColor "green" ""
                , ppVisible         = wrap "(" ")"
                , ppUrgent          = xmobarColor "red" "yellow" . wrap "!" "!"
                }) xmprocs
        , borderWidth        = 2
        , terminal           = myTerminal
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        , modMask            = mod4Mask
        , workspaces         = map show ([1..5] :: [Int])
        , startupHook        = startupHook def <+> setFullscreenSupported
        } `additionalKeysP` myKeys
    where noScratchPad ws = if ws == "NSP" then "" else ws
          tall = Tall 1 (3/100) (1/2)
          getMainWorkspace n = if n > 1 then "1" else "2"

myXPromptConfig :: XPConfig
myXPromptConfig = def
    { font = "xft: Terminus:size=13"
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
myUrgencyHook = withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "red" } urgencyConfig { suppressWhen = Focused }

myTerminal :: String
myTerminal = "alacritty"

myKeys :: [(String, X ())]
myKeys = [ ( "<XF86AudioPlay>", spawn "playerctl play-pause" )
         , ( "<XF86AudioNext>", spawn "playerctl next" )
         , ( "<XF86AudioPrev>", spawn "playerctl previous" )
         , ( "<XF86AudioStop>", spawn "playerctl stop" )
         , ( "<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%" )
         , ( "<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%" )
         , ( "<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle" )
         , ( "<XF86AudioMicMute>", spawn "pactl set-source-mute 1 toggle" )
         , ( "<XF86MonBrightnessUp>", spawn "xbacklight -fps 60 +7.5" )
         , ( "<XF86MonBrightnessDown>", spawn "xbacklight -fps 60 -7.5" )
         , ( "M4-p", shellPrompt myXPromptConfig )
         , ( "M4-m", manPrompt myXPromptConfig )
         , ( "C-M1-l", spawn "light-locker-command -l" )
         , ( "<F12>", scratchPad )
         , ( "<XF86Display>", spawn "arandr" )
         , ( "<XF86Tools>", spawn "blueman-manager" )
         , ( "<Print>", spawn "gnome-screenshot --interactive" )
         , ( "M4-b", sendMessage ToggleStruts )
         , ( "M4-<Backspace>", focusUrgent )
         ]
    where scratchPad = namedScratchpadAction myScratchPads "terminal"

myManageHook :: ManageHook
myManageHook = do
    n <- countScreens
    let desktop = if (n :: Int) > 1 then "1" else "2" in
        composeAll
            [ isDialog --> doCenterFloat
            , className =? "alacritty" <&&> title =? "neomutt" --> doShift desktop
            , className =? "firefox" <&&> title =? "Picture-in-Picture" --> doFloat
            , className =? "gcr-prompter" <||> className =? "Gcr-prompter" --> doCenterFloat
            , className =? "discord" --> doShift desktop
            , className =? "hexchat" <||> className =? "Hexchat" --> doShift desktop
            , className =? "telegram-desktop" <||> className =? "TelegramDesktop" --> doShift desktop
            , (className =? "telegram-desktop" <||> className =? "TelegramDesktop") <&&> title =? "Media viewer" --> doCenterFloat
            ]

myDynHook :: ManageHook
myDynHook = do
    n <- countScreens
    let desktop = if (n :: Int) > 1 then "1" else "2" in
        composeAll
            [ className =? "spotify" <||> className =? "Spotify" --> doShift desktop
            ]

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm  findTerm  manageTerm
                ]
    where
        spawnTerm  = myTerminal ++ " --title Scratchpad"
        findTerm   = title =? "Scratchpad" <&&> className =? "Alacritty"
        manageTerm = customFloating $ W.RationalRect l t w h
            where
                h = 0.35
                w = 1
                t = 1 - h - 0.016
                l = 1 - w

-- hack to let firefox run fullscreen
setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom
                [ "_NET_WM_STATE_HIDDEN"
                , "_NET_WM_STATE_FULLSCREEN" -- XXX Copy-pasted to add this line
                , "_NET_NUMBER_OF_DESKTOPS"
                , "_NET_CLIENT_LIST"
                , "_NET_CLIENT_LIST_STACKING"
                , "_NET_CURRENT_DESKTOP"
                , "_NET_DESKTOP_NAMES"
                , "_NET_ACTIVE_WINDOW"
                , "_NET_WM_DESKTOP"
                , "_NET_WM_STRUT"
                ]

    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)
