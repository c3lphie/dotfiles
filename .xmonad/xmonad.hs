-- Default
import XMonad
import XMonad.Operations
import System.Exit
import qualified XMonad.StackSet as W

-- Data
import Data.Monoid
import qualified Data.Map        as M
import Data.Tree

-- Hooks
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

-- Util
import XMonad.Util.EZConfig
import XMonad.Util.Run (hPutStrLn, runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Prompt.FuzzyMatch

-- Actions
import XMonad.Actions.Promote
import qualified XMonad.Actions.TreeSelect as TS

-- Layouts
import XMonad.Layout
import XMonad.Layout.Tabbed
import XMonad.Layout.LimitWindows
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders

myTerminal      = "st"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 1

-- Font
myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#44475a"
myFocusedBorderColor = "#f8f8f2"

-- Prompt config
myXPConfig :: XPConfig
myXPConfig = def
  {
    bgColor             = "#292d3e",
    fgColor             = "#d0d0d0",
    bgHLight            = "#c792ea",
    fgHLight            = "#000000",
    borderColor         = "#535974",
    promptBorderWidth   = 0,
    position            = Top,
    height              = 20,
    historySize         = 256,
    historyFilter       = id,
    defaultText         = [],
    autoComplete        = Just 100000,
    showCompletionOnTab = False,
    searchPredicate     = fuzzyMatch,
    alwaysHighlight     = True,
    maxComplRows        = Nothing
  }

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Editor
myEditor :: String
myEditor = "emacs"

-- Browser
myBrowser :: String
myBrowser = "brave"

myModMask       = mod4Mask

myKeys :: [(String, X())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad

    -- Open my preferred terminal
        , ("M-<Return>", spawn myTerminal)

    -- Open my preferred texteditor
        , ("M-e", spawn myEditor)

    -- Open treeselect for system things
        , ("M-<Backspace>", treeselectAction myTreeConf)

    -- Open browser
        , ("M-w", spawn myBrowser)

    -- Run Prompt
        , ("M-d", spawn "dmenu_run")   -- Shell Prompt

    -- Windows
        , ("M-q", kill)                      -- Kill the currently focused client
        , ("M-m", windows W.focusMaster)     -- Move focus to the master window
        , ("M-j", windows W.focusDown)       -- Move focus to the next window
        , ("M-k", windows W.focusUp)         -- Move focus to the prev window
        , ("M-S-j", windows W.swapDown)      -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)        -- Swap focused window with prev window
        , ("M-<Space>", promote)             -- Moves focused window to master, others maintain order

    -- Layouts management
        , ("M-<Tab>", sendMessage NextLayout)

         ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

tall    = renamed [Replace "tall"]
          $ limitWindows 12
          $ ResizableTall 1 (3/100) (1/2) []
tabs    = renamed [Replace "tabs"]
          $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def {   fontName            = "xft:Monoki Nerd Font:regular:pixelsize=13",
                          activeColor         = "#292d3e",
                          inactiveColor       = "#3e445e",
                          activeBorderColor   = "#292d3e",
                          inactiveBorderColor = "#292d3e",
                          activeTextColor     = "#ffffff",
                          inactiveTextColor   = "#d0d0d0"
                        }


myLayoutHook = avoidStruts myDefaultLayout
  where
    myDefaultLayout =   tall
                    ||| noBorders tabs

-- TreeSelect configuration
myTreeConf :: TS.TSConfig a
myTreeConf = TS.TSConfig {
  TS.ts_hidechildren = True,
  TS.ts_background   = 0x44475a,
  TS.ts_font         = myFont,
  TS.ts_node         = (0xffd0d0d0, 0xff202331),
  TS.ts_nodealt      = (0xffd0d0d0, 0xff292d3e),
  TS.ts_highlight    = (0xffffffff, 0xffbd93f9),
  TS.ts_extra        = 0xffd0d0d0,
  TS.ts_node_width   = 200,
  TS.ts_node_height  = 20,
  TS.ts_originX      = 0,
  TS.ts_originY      = 0,
  TS.ts_indent       = 80,
  TS.ts_navigate     = myTreeNavigation
                          }

-- TreeSelect navigation
myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    ]

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
  [ Node (TS.TSNode "Lock" [] (spawn "i3lock-fancy")) [],
    Node (TS.TSNode "Suspend" [] (spawn "systemctl hybrid-sleep")) [],
    Node (TS.TSNode "Shutdown" [] (spawn "shutdown -h now")) [],
    Node (TS.TSNode "Restart" [] (spawn "reboot")) []
  ]

myWorkspaces    = ["dev","www","hack","music"] ++ map show [5..9]
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

myLogHook = return ()

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --bg-fill /home/c3lphie/wallpapers "
  spawnOnce "emacs --daemon &"
  spawnOnce "picom &"

main = do
  xmproc <- spawnPipe "xmobar /home/c3lphie/.xmonad/xmobarrc"
  xmonad $ def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = myEventHook <+> docksEventHook,
        logHook            = myLogHook <+> dynamicLogWithPP xmobarPP
        {
          ppOutput = \x -> hPutStrLn xmproc x
        , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
        , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
        , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"                     -- Separators in xmobar
        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
        , ppExtras  = [windowCount]                           -- # of windows current workspace
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        },
        startupHook        = myStartupHook
    } `additionalKeysP` myKeys
