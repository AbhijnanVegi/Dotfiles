
  -- Base
import XMonad hiding ( (|||) )
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), toggleWS, moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.Search as S

    -- Prompts
import XMonad.Prompt
import XMonad.Prompt.XMonad

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ServerMode

    -- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutCombinators
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

   -- ColorScheme module (SET ONLY ONE!)
      -- Possible choice are:
      -- DoomOne
      -- Dracula
      -- GruvboxDark
      -- MonokaiPro
      -- Nord
      -- OceanicNext
      -- Palenight
      -- SolarizedDark
      -- SolarizedLight
      -- TomorrowNight
import Colors.Dracula

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "kitty"    -- Sets default terminal

myBrowser :: String
myBrowser = "brave"  -- Sets qutebrowser as browser

myEditor :: String
myEditor = "code"    -- Sets vscode as editor

myBorderWidth :: Dimension
myBorderWidth = 1           -- Sets border width for windows

myNormColor :: String       -- Border color of normal windows
myNormColor   = colorBack   -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = color05     -- This variable is imported from Colors.THEME

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
--  spawn "killall conky"   -- kill current conky on each restart
    spawn "killall trayer"  -- kill current trayer on each restart

    spawnOnce "picom"
    spawnOnce "nm-applet"
    spawnOnce "blueman-applet"
    spawnOnce "volumeicon"
    spawnOnce "dunst"
    spawnOnce "/usr/lib/polkit-kde-authentication-agent-1"
    spawnOnce "~/.local/bin/add-monitor.sh"
    spawnOnce "eww daemon"
    spawnOnce "discord"

--  spawn ("sleep 2 && conky -c $HOME/.config/conky/xmonad/" ++ colorScheme ++ "-01.conkyrc")
    spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 " ++ colorTrayer ++ " --height 22")

    spawnOnce "~/.fehbg &"  -- set last saved feh wallpaper
    -- spawnOnce "feh --randomize --bg-fill ~/wallpapers/*"  -- feh set random wallpaper
    -- spawnOnce "nitrogen --restore &"   -- if you prefer nitrogen to feh
    setWMName "LG3D"

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "ranger" spawnRanger findRanger manageRanger
                , NS "discord" spawnDiscord findDiscord manageDiscord
                , NS "spotify" spawnSpotify findSpotify manageSpotify
                , NS "outlook" spawnOutlook findOutlook manageOutlook
                ]
  where
    spawnTerm  = myTerminal ++ " -T scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.75 -h
                 l = 0.75 -w

    spawnRanger = myTerminal ++ " -T ranger -e ranger"
    findRanger = title =? "ranger"
    manageRanger = customFloating $ W.RationalRect l t w h
                where
                  h = 0.5
                  w = 0.5
                  t = 0.75 - h
                  l = 0.75 - w
    spawnDiscord = "discord"
    findDiscord = className =? "discord"
    manageDiscord = customFloating $ W.RationalRect l t w h
              where
                  h = 0.8
                  w = 0.8
                  t = 0.1
                  l = 0.1
    spawnSpotify = "~/Scripts/spotify-adblock"
    findSpotify = className =? "Spotify"
    manageSpotify = customFloating $ W.RationalRect l t w h
              where
                  h = 0.6
                  w = 0.6
                  t = 0.2
                  l = 0.2
    spawnOutlook = "gtk-launch Outlook"
    findOutlook = title =? "Outlook (PWA) - Mail - Vegi Abhijnan - Outlook"
    manageOutlook = customFloating $ W.RationalRect l t w h
              where
                  h = 0.6
                  w = 0.6
                  t = 0.2
                  l = 0.2

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
--magnify  = renamed [Replace "magnify"]
--           $ smartBorders
  --         $ windowNavigation
    --       $ addTabs shrinkText myTabTheme
      --     $ subLayout [] (smartBorders Simplest)
        --   $ magnifier
          -- $ limitWindows 12
           -- $ mySpacing 8
              -- $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing' 8
           $ spiral (6/7)
--threeCol = renamed [Replace "threeCol"]
--         $ smartBorders
--         $ windowNavigation
--         $ addTabs shrinkText myTabTheme
--         $ subLayout [] (smartBorders Simplest)
--         $ limitWindows 7
--         $ ThreeCol 1 (3/100) (1/2)
--threeRow = renamed [Replace "threeRow"]
--         $ smartBorders
--         $ windowNavigation
--         $ addTabs shrinkText myTabTheme
--         $ subLayout [] (smartBorders Simplest)
--         $ limitWindows 7
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
--         $ Mirror
--         $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabTheme
tallAccordion  = renamed [Replace "tallAccordion"]
           $ Accordion
wideAccordion  = renamed [Replace "wideAccordion"]
           $ Mirror Accordion

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = color15
                 , inactiveColor       = color08
                 , activeBorderColor   = color15
                 , inactiveBorderColor = colorBack
                 , activeTextColor     = colorBack
                 , inactiveTextColor   = color16
                 }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 -- ||| magnify
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| noBorders tabs
                                --  ||| grid
                                 ||| spirals
--                               ||| threeCol
--                               ||| threeRow
                                --  ||| tallAccordion
                                --  ||| wideAccordion

myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
-- myWorkspaces = [" dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces and the names would be very long if using clickable workspaces.
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "Gimp"            --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "pinentry-gtk-2"  --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
     , className =? "Yad"             --> doCenterFloat
     , className =? "notion-app-enhanced"--> doShift ( myWorkspaces !! 8 )
     , title =? "Android Emulator.*"  --> doFloat
     , title =? "Picture in picture"  --> doFloat
     , title =? "MyGL"                --> doFloat
     , title =? "CP"                  --> doShift ( myWorkspaces !! 2 )
     , title =? "Home"                --> doShift ( myWorkspaces !! 0 )
--   , className =? "Brave-browser"   --> doShift ( myWorkspaces !! 1 )
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
     , isFullscreen -->  doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads

-- START_KEYS
myKeys :: [(String, X ())]
myKeys =
    -- KB_GROUP Xmonad
        [ ("M-S-r", spawn "xmonad --recompile; xmonad --restart")       -- Recompiles xmonad
        , ("M-S-q", io exitSuccess)                   -- Quits xmonad

    -- KB_GROUP Run Prompt
        , ("M-S-<Return>", xmonadPrompt def {
                   position = Top
                 , promptBorderWidth = 0
                 , defaultText = ""
                 , alwaysHighlight = True
                 , height = 32
                 , font = "xft:DejaVu Sans Condensed-10:normal"
                 }) -- Dmenu

    -- KB_GROUP Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn (myTerminal))
        , ("M-b", spawn (myBrowser))
        , ("M-e", spawn (myEditor))
        , ("M-M1-h", spawn (myTerminal ++ " -e gotop"))
        , ("M-d", spawn "rofi -show drun -modi drun")
        , ("M-S-s", spawn "flameshot gui")
        , ("M-M1-l", spawn "betterlockscreen -l")
        , ("M-p", spawn "~/.local/bin/dash.sh")
        , ("M-S-p", spawn "eww close-all")

    -- KB_GROUP Kill windows
        , ("M-q", kill1)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace

    -- KB_GROUP Workspaces
        , ("M1-`", nextScreen)  -- Switch focus to next monitor
--      , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- KB_GROUP Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- KB_GROUP Increase/decrease spacing (gaps)
        , ("C-M1-j", decWindowSpacing 4)         -- Decrease window spacing
        , ("C-M1-k", incWindowSpacing 4)         -- Increase window spacing
        , ("C-M1-h", decScreenSpacing 4)         -- Decrease screen spacing
        , ("C-M1-l", incScreenSpacing 4)         -- Increase screen spacing

    -- KB_GROUP Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M1-<Tab>", spawn "rofi -show window") -- Alternate move focus
        , ("M-<Tab>", toggleWS)
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- KB_GROUP Layouts
        , ("M-`", sendMessage NextLayout)           -- Switch to next layout
        , ("M-S-`", sendMessage $ JumpToLayout "tall")
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- KB_GROUP Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase # of clients master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase # of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease # of windows

    -- KB_GROUP Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width

    -- KB_GROUP Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-C-h", sendMessage $ pullGroup L)
        , ("M-C-l", sendMessage $ pullGroup R)
        , ("M-C-k", sendMessage $ pullGroup U)
        , ("M-C-j", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        -- , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-n", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- KB_GROUP Scratchpads
    -- Toggle show/hide these programs.  They run on a hidden workspace.
    -- When you toggle them to show, it brings them to your current workspace.
    -- Toggle them to hide and it sends them back to hidden workspace (NSP).
        , ("M--", namedScratchpadAction myScratchPads "terminal")
        , ("M-r", namedScratchpadAction myScratchPads "ranger")
        , ("M-w d", namedScratchpadAction myScratchPads "discord")
        , ("M-w s", namedScratchpadAction myScratchPads "spotify")
        , ("M-w m", namedScratchpadAction myScratchPads "outlook")

       -- KB_GROUP Multimedia Keys
        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
        , ("<XF86AudioPrev>", spawn "playerctl previous")
        , ("<XF86AudioNext>", spawn "playerctl next")
        , ("<XF86AudioMute>", spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "brave")
        , ("<XF86Search>", spawn "brave")
--      , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
--      , ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
--      , ("<XF86Eject>", spawn "toggleeject")
--      , ("<Print>", spawn "dm-maim")
        ]
    -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
-- END_KEYS

-- SERVER_MODE SETUP
myCommands :: [(String, X ())]
myCommands =
        [ ("decrease-master-size"      , sendMessage Shrink                               )
        , ("increase-master-size"      , sendMessage Expand                               )
        , ("decrease-master-count"     , sendMessage $ IncMasterN (-1)                    )
        , ("increase-master-count"     , sendMessage $ IncMasterN ( 1)                    )
        , ("focus-prev"                , windows W.focusUp                                )
        , ("focus-next"                , windows W.focusDown                              )
        , ("focus-master"              , windows W.focusMaster                            )
        , ("swap-with-prev"            , windows W.swapUp                                 )
        , ("swap-with-next"            , windows W.swapDown                               )
        , ("swap-with-master"          , windows W.swapMaster                             )
        , ("kill-window"               , kill                                             )
        , ("quit"                      , io $ exitSuccess                        )
        , ("restart"                   , spawn "xmonad --recompile; xmonad --restart"     )
        ]

myServerModeEventHook = serverModeEventHookCmd' $ return myCommands'
myCommands' = ("list-commands", listMyServerCmds) : myCommands ++ wscs -- ++ sccs -- ++ spcs
    where
        wscs = [((m ++ show i), windows $f s) | (s,i) <- zip myWorkspaces [1..]
               , (f, m) <- [(W.view, "focus-"), (W.shift, "send-")] ]

        -- sccs = [((m ++ show sc), screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
        --        | sc <- [0..myMaxScreenCount], (f, m) <- [(W.view, "focus-screen-"), (W.shift, "send-to-screen-")]]

--        spcs = [("toggle-" ++ sp, namedScratchpadAction myScratchpads sp)
--               | sp <- (flip map) (myScratchpads) (\(NS x _ _ _) -> x) ]
listMyServerCmds :: X ()
listMyServerCmds = spawn ("echo '" ++ asmc ++ "' | xmessage -file -")
    where asmc = concat $ "Available commands:" : map (\(x, _)-> "\n" ++ x) myCommands'

main :: IO ()
main = do
    -- Launching xmobar
    xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
    -- xmproc0 <- spawnPipe ("xmolog -b")
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh $ ewmhFullscreen $ docks def
        { manageHook         = myManageHook <+> manageDocks
        -- , handleEventHook    = docks <+> myServerModeEventHook 
        , handleEventHook    = windowedFullscreenFixEventHook
                               -- Uncomment this line to enable fullscreen support on things like YouTube/Netflix.
                               -- This works perfect on SINGLE monitor systems. On multi-monitor systems,
                               -- it adds a border around the window if screen does not have focus. So, my solution
                               -- is to use a keybinding to toggle fullscreen noborders instead.  (M-<Space>)
                                -- <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP ( filterOutWsPP [scratchpadWorkspaceTag] xmobarPP   
              -- XMOBAR SETTINGS
              { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
                -- Current workspace
              , ppCurrent = xmobarColor color06 "" . wrap
                            ("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">") "</box>"
                -- Visible but not current workspace
              , ppVisible = xmobarColor color06 "" . clickable
                -- Hidden workspace
              , ppHidden = xmobarColor color05 "" . wrap
                           ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">") "</box>" . clickable
                -- Hidden workspaces (no windows)
              , ppHiddenNoWindows = xmobarColor color05 ""  . clickable
                -- Title of active window
              , ppTitle = xmobarColor color16 "" . shorten 60
                -- Separator character
              , ppSep =  "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
                -- Urgent workspace
              , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
                -- Adding # of windows on current workspace to the bar
              , ppExtras  = [windowCount]
                -- order of things in xmobar
              , ppOrder  = \(ws:l:t:ex) -> [ws]
              } )


              -- XMOLOG SETTINGS
              -- {   ppCurrent          = wrap "[" "]"
              --   , ppVisible          = wrap "<" ">"
              --   , ppHidden           = id
              --   , ppHiddenNoWindows  = const ""
              --   , ppVisibleNoWindows = Nothing
              --   , ppUrgent           = wrap "!" "!"
              --   , ppRename           = pure
              --   , ppSep              = " : "
              --   , ppWsSep            = " "
              --   , ppTitle            = shorten 80
              --   , ppTitleSanitize    = xmobarStrip . dzenEscape
              --   , ppLayout           = id
              --   , ppOrder            = id
              --   , ppOutput           = \x -> hPutStrLn xmproc0 x
              --   , ppExtras           = []
              -- })

--              >> updatePointer (0.5,0.5) (0,0)
        } `additionalKeysP` myKeys
