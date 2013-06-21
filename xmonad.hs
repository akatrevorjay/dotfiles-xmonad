import XMonad

import XMonad.Config.Desktop
import XMonad.Config.Gnome
--import XMonad.Config.Azerty

import XMonad.Hooks.DynamicHooks
--import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.DynamicLog ( PP(..), dynamicLogWithPP, dzenColor, wrap, defaultPP )
import XMonad.Hooks.ManageDocks

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Theme
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.Ssh

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
--import XMonad.Actions.UpdatePointer
--import XMonad.Actions.Plane

import XMonad.Layout.LayoutHints
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
--import XMonad.Layout.HintedTile
import XMonad.Layout.Gaps
import XMonad.Layout.Named
--import XMonad.Layout.HintedGrid
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Maximize
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
--import XMonad.Layout.DwmStyle
import XMonad.Layout.Dishes

import XMonad.Layout.LayoutCombinators hiding ( (|||) )

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce
--import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.EZConfig
import XMonad.Util.Themes
--import XMonad.Util.Scratchpad
--import XMonad.Util.Dzen

import Data.Ratio((%))
import Data.Monoid
import System.IO

import qualified Data.Map as M
import qualified XMonad.StackSet as W

--myManageHook = scratchpadManageHook (W.RationalRect 0.125 0.125 0.75 0.75) <+>
myManageHook = composeAll
	[ className =? "ClockScreenlet.py"	--> doFloat
--	, className =? "Gnome-panel"		--> doFloat
	, className =? "rdesktop"			--> doFloat
	, className =? "Xmessage"			--> doFloat
	, className =? "Tilda"				--> doFloat
	, className =? "mplayer"			--> doFloat
	, className =? "gmplayer"			--> doFloat
	, className =? "vlc"				--> doFloat
	, className =? "glxgears"			--> doFloat
	, className =? "Vncviewer"			--> doFloat
	, className =? "com-limegroup-gnutella-gui-Main" --> doFloat
	-- , className =? "Vmware"			--> doFullFloat
	, className =? "Unity-2d-panel"	--> doIgnore
	, className =? "Unity-2d-launcher" --> doIgnore
    , className =? "Conky" --> doIgnore
	, className =? "xmobar" --> doIgnore
	, className =? "dzen2" --> doIgnore
	, className =? "Guake.py" --> doFloat
    , className =? "processing-app-Base" --> doFloat
    , className =? "Xfce4-notifyd" --> doIgnore
	, isFullscreen --> doFullFloat
	, isDialog --> doCenterFloat
	]

mylayoutHook =
	( desktopLayoutModifiers -- $ dwmStyle shrinkText (theme xmonadTheme)
	-- $ gaps [(U, 24)]
	$ maximize $ smartBorders
	$ withIM (0.11) (Role "gimp-toolbox")
	$ reflectHoriz $ withIM (0.15) (Role "gimp-dock") $ reflectHoriz

	$ tall ||| tabbed ||| grid1 ||| threecol ||| accordion ||| spiral1 ||| hinttall ||| centhinttall ||| fullcenthinttall ||| tiled ||| dishes
	)
	where
		tall = Tall 1 (3/100) (1/2)
		tabbed = simpleTabbedBottom
		-- HintedGrid
		--grid1 = Grid False
		-- Grid
		grid1 = Grid
		threecol = ThreeCol 1 (3/100) (1/2)
		accordion = Accordion
		spiral1 = spiral (6/7)
		hinttall = layoutHints (Tall 1 (3/100) (1/2))
		centhinttall = layoutHintsToCenter (Tall 1 (3/100) (1/2))
		fullcenthinttall = layoutHintsToCenter (Tall 1 (3/100) (1/2))
		tiled = Tall nmaster delta tiled_ratio
		dishes = Dishes nmaster dishes_ratio
		nmaster = 1
		delta = 1/100
		tiled_ratio = 1/2
		dishes_ratio = 1/5


fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
centerFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doCenterFloat f

--keymap
myEZKeysP = myEZKeymap (myConfig)
myEZKeymap conf =
	-- [ ("M-<Backspace>", scratchpadSpawnAction conf ) -- how do I get twosuperior?
	[ ("M-<Backspace>", withFocused hide) -- N.B. this is an absurd thing to do
	]

myEZKeys =
	[ ((mod4Mask, xK_grave), spawn "gnome-terminal")
	, ((mod4Mask .|. shiftMask, xK_grave), spawn "gnome-terminal -e byobu")
	, ((mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command -l")
	, ((mod4Mask .|. shiftMask, xK_g), spawn "gtg")
	, ((mod4Mask .|. shiftMask, xK_t), spawn "hamster-time-tracker")

	-- , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
	-- , ((0, xK_Print), spawn "scrot")

	-- moving workspaces
	, ((mod4Mask, xK_Left), prevWS )
	, ((mod4Mask, xK_Right), nextWS )
	, ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev )
	, ((mod4Mask .|. shiftMask, xK_Right), shiftToNext )

	-- tranparency via transset
	--, ((mod4Mask, xK_Up), spawn "transset-df -a --dec .1")
	--, ((mod4Mask, xK_Down), spawn "transset-df -a --inc .1")

	-- layout Maximize
	, ((mod4Mask, xK_a), withFocused (sendMessage . maximizeRestore))
	-- fullfloat
	, ((mod4Mask, xK_f), fullFloatFocused)
	, ((mod4Mask, xK_c), centerFloatFocused)
	, ((mod4Mask, xK_r), runOrRaisePrompt defaultXPConfig)
	, ((mod4Mask, xK_w), kill)
	--, ((mod4Mask, xK_t), themePrompt defaultXPConfig)

	--, ((mod4Mask .|. shiftMask, xK_a), renameWorkspace defaultXPConfig)
	--, ((mod4Mask, xK_d), viewEmptyWorkspace)
	, ((mod4Mask, xK_x), shellPrompt defaultXPConfig)
	, ((mod4Mask .|. shiftMask, xK_x), xmonadPrompt defaultXPConfig)
	, ((mod4Mask .|. shiftMask, xK_v), windowPromptBring defaultXPConfig)
	, ((mod4Mask, xK_v), windowPromptGoto defaultXPConfig)
	, ((mod4Mask, xK_u), workspacePrompt defaultXPConfig (windows . W.greedyView))
	, ((mod4Mask, xK_s), sshPrompt defaultXPConfig)
	]
	-- ++ M.toList (planeKeys mod4Mask GConf Finite)

myLogHook_dzen h = dynamicLogWithPP $ defaultPP {
			  ppCurrent  = dzenColor "#ffffff" "#000000" . pad
			, ppVisible  = dzenColor "#888888" "#000000" . pad
			, ppHidden   = dzenColor "#888888" "#000000" . pad
			--, ppHiddenNoWindows = dzenColor "#888888"  "#000000" . pad
			, ppSep		 = " | "
			, ppUrgent   = dzenColor "#ff0000" "#000000"
			, ppOutput   = hPutStrLn h
			}
			where
	  			fill :: String -> Int -> String
	  			fill h i = "^p(" ++ show i ++ ")" ++ h ++ "^p(" ++ show i ++ ")"

-- Color, font and iconpath definitions:
--myFont = "-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1"
--myFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-c-*-*-*"
--myFont = "-misc-fixed-*-r-normal-*-10-*-*-*-*-*-*-*"
--myFont = "Envy Code R for Powerline"
myFont = "Fixed"
myIconDir = "/home/trevorj/.dzenstatus/bitmaps"
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
mySeperatorColor = "#555555"
{-myEvents = "-l 10 -u -m"-}
myEvents = ""
--myStatusBar = "dzen2 -ta l -p -x 0 -y 0 -w 1897 -h 25 -fn '-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*'"
myStatusBar = "dzen2 -x '400' -y '0' -h '16' -w '900' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' " ++ myEvents
myLogHook_dzen2 h = dynamicLogWithPP $ defaultPP {
	  ppCurrent  = dzenColor "#222222" "white" . pad
	, ppVisible  = dzenColor "white" "black" . pad
	, ppHidden   = dzenColor "lightblue" "#222222" . pad
	--, ppHiddenNoWindows = dzenColor "#777777"  "#222222" . pad
	, ppUrgent   = dzenColor "red" "yellow"
--	, ppWsSep	= " | "
	, ppSep	  = " | "
	, ppLayout   = dzenColor "lightblue" "#222222"
				{-(\ x -> fill (case x of					-}
				{-   "Tall"	   -> icon "tall.xbm"			-}
				{-   "Mirror Tall"		-> icon "mtall.xbm"-}
				{-   "Full"	   -> icon "full.xbm"			-}
				{-   _			-> pad x) 4)				  -}
	, ppTitle	= dzenEscape
	, ppOutput   = hPutStrLn h
	}
	where
		--icon h = "^i(/home/edgar/dzen_bitmaps/" ++ h ++ ")"
		--fill :: String -> Int -> String
		--fill h i = "^p(" ++ show i ++ ")" ++ h ++ "^p(" ++ show i ++ ")"

--myStatusBar = "xmobar"
myLogHook_xmobar h = (dynamicLogWithPP $ xmobarPP
  { ppOutput = hPutStrLn h
  , ppTitle  = xmobarColor "green" "" . shorten 50
  --, ppSort   = fmap (.scratchpadFilterOutWorkspace) $ ppSort xmobarPP
  })-- >> updatePointer (TowardsCentre 1 1)


--myStatusBar = "dzenstatus"
myLogHook_dzenstatus h = dynamicLogWithPP $ defaultPP


myConfig = ewmh gnomeConfig
	{ manageHook = myManageHook <+> manageHook gnomeConfig
	, layoutHook = mylayoutHook
	, modMask = mod4Mask
	, terminal = "gnome-terminal"
	--, keys = \c -> azertyKeys c `M.union` keys gnomeConfig c
	, workspaces = ["sh", "web", "todo", "four", "five", "six", "seven", "eight", "nine"]
	{-, normalBorderColor   = colorNormalBorder-}
	{-, focusedBorderColor  = colorFocusedBorder-}
	{-, borderWidth		 = 2-}
	}
	`additionalKeysP` myEZKeysP
	`additionalKeys` myEZKeys

myStartupHook = do
  setDefaultCursor xC_pirate
  --spawnOnce "/home/trevorj/.bin/polaris.py"
  --spawnOnce "/home/trevorj/.bin/pydzen.py"
  --spawnOnce "gtg"
  --spawnOnce "hamster-indicator"
  --spawnOnce "hamster-time-tracker"
  --spawnOnce "pidgin"
  --spawnOnce "gnome-terminal"
  --spawnOnce "~/.bin/chromium-browser"
  --spawnOnce "cairo-compmgr"


main = do
	-- dzen <- spawnPipe myStatusBar
	-- polaris <- spawnPipe "python /home/trevorj/.bin/polaris.py"
	-- pydzen <- spawnPipe "python /home/trevorj/.bin/pydzen.py"
	xmonad $ myConfig {
		-- logHook = myLogHook_dzenstatus dzen >> logHook desktopConfig
		-- logHook = myLogHook_dzen2 dzen >> logHook desktopConfig
		-- logHook = myLogHook_xmobar xmproc >> logHook desktopConfig
		-- borderWidth = 1
		startupHook = myStartupHook
		}


