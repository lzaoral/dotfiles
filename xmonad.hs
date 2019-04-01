import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.IndependentScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import XMonad.Util.Run ( spawnPipe )
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import System.IO

main = do
    n <- countScreens
    xmprocs <- mapM (\i -> spawnPipe $ "xmobar ~/.xmonad/xmobar-" ++ show i ++ ".hs -x " ++ show i) [0..n-1]
    xmonad $ ewmh def
        { manageHook         = myManageHook
        , layoutHook         = avoidStruts $ smartBorders $ layoutHook def
        , handleEventHook    = dynamicPropertyChange "WM_NAME" myManageHook <+> fullscreenEventHook 
                               <+> docksEventHook
        , logHook            = mapM_ (\handle -> dynamicLogWithPP $ xmobarPP
                { ppOutput          = hPutStrLn handle
                , ppCurrent         = xmobarColor "yellow" "" . wrap "[" "]"
                , ppHiddenNoWindows = xmobarColor "grey" "" . noScratchPad
                , ppHidden          = noScratchPad
                , ppTitle           = xmobarColor "green" ""
                , ppVisible         = wrap "(" ")"
                , ppUrgent          = xmobarColor "red" "yellow"
                }) xmprocs
        , borderWidth        = 2
        , terminal           = myTerminal
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        , modMask            = mod4Mask
        , workspaces         = map show [1..5]
        , startupHook        = startupHook def <+> setFullscreenSupported
        } `additionalKeysP` myKeys
    where noScratchPad ws = if ws == "NSP" then "" else ws

myTerminal = "urxvt" -- "gnome-terminal"

myKeys = [ ( "<XF86AudioPlay>", spawn "playerctl play-pause" )
         , ( "<XF86AudioNext>", spawn "playerctl next" )
         , ( "<XF86AudioPrev>", spawn "playerctl previous" )
         , ( "<XF86AudioStop>", spawn "playerctl stop" )
         , ( "<XF86MonBrightnessUp>", spawn "xbacklight -inc 5" )
         , ( "<XF86MonBrightnessDown>", spawn "xbacklight -dec 5" )
         , ( "C-M1-l", spawn "slock" )
         , ( "C-M1-t", scratchPad )
         ]
    where scratchPad = scratchpadSpawnActionTerminal myTerminal

myManageHook = (isDialog --> doCenterFloat) <+> manageDocks <+> manageScratchPad <+>
    composeAll
        [ className =? "TelegramDesktop" <&&> title =? "Media viewer" --> doCenterFloat
        , className =? "TelegramDesktop" --> doShift "2"
        , className =? "spotify"         --> doShift "2"
        ]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
    where h = 0.30           -- terminal height, 10%
          w = 1              -- terminal width, 100%
          t = 1 - h - 0.016  -- distance from top edge, 90%
          l = 1 - w          -- distance from left edge, 0%

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
    setWMName "LG3D"
