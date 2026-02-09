import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import XMonad
-- Hooks

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
-- Layouts

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.StackSet qualified as W
-- Keybindings
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Variables
myTerminal = "kitty"
myBrowser = "firefox"
myEditor = "codium"
myLauncher = "rofi -show drun -drun-match-fields name"
myModMask = mod4Mask -- Super

-- Colors
myNormColor = "#f8f8f2"
myFocusColor = "#bd93f9"
myBorderWidth = 2

-- Workspaces
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8"]

-- Layouts Definition
myLayout = avoidStruts $ smartBorders $ mySpacing $ tiled ||| Full
  where
    tiled = ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100
    mySpacing =
      spacingRaw
        False
        (Border 5 5 5 5)
        True
        (Border 5 5 5 5)
        True

-- Startup Hook
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "picom &"
  spawnOnce "nm-applet &"
  spawn "/home/venpy/.config/polybar/launch.sh"

-- Keybindings
myKeys :: [(String, X ())]
myKeys =
  -- Applications
  [ ("M-<Return>", spawn myTerminal),
    ("M-<Space>", spawn myLauncher),
    ("M-c", spawn myEditor),
    ("M-i", spawn myBrowser),
    ("M-o", spawn "obsidian"),
    ("M-n", spawn "thunar"),

    -- System
    ("M-<Escape>", spawn "xmonad --recompile; xmonad --restart"),
    ("M-S-q", io exitSuccess),
    ("M-w", kill),

    -- Layouts & Focus
    ("M-m", sendMessage NextLayout), -- Toggle Layout (Tile/Full)
    ("M-S-d", sendMessage (IncMasterN 1)), -- Increase windows in master
    ("M-S-a", sendMessage (IncMasterN (-1))), -- Decrease windows in master
    ("M-t", withFocused $ windows . W.sink), -- Force floating to tile

    -- Focus movement (Vim style)
    ("M-j", windows W.focusDown),
    ("M-k", windows W.focusUp),
    ("M-h", sendMessage Shrink), -- Resize Master (Shrink)
    ("M-l", sendMessage Expand), -- Resize Master (Expand)

    -- Window swapping
    ("M-S-j", windows W.swapDown),
    ("M-S-k", windows W.swapUp),
    ("M-S-m", windows W.swapMaster), -- Swap current with master

    -- Resizing
    ("M-A-j", sendMessage MirrorShrink),
    ("M-A-k", sendMessage MirrorExpand),
    -- Power & Session
    ("M-C-p", spawn "sudo poweroff"), -- add to sudoers for passwordless execution
    ("M-C-r", spawn "sudo reboot"),
    ("M-C-s", spawn "slock"), -- todo

    -- Hardware Keys
    ("<XF86MonBrightnessUp>", spawn "sudo brightnessctl s +10%"),
    ("<XF86MonBrightnessDown>", spawn "sudo brightnessctl s 10%-"),
    ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5"),
    ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5"),
    ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
  ]

-- Main Config
main :: IO ()
main =
  xmonad $
    ewmhFullscreen $
      ewmh $
        docks $
          def
            { terminal = myTerminal,
              focusFollowsMouse = True,
              clickJustFocuses = False,
              borderWidth = myBorderWidth,
              modMask = myModMask,
              workspaces = myWorkspaces,
              normalBorderColor = myNormColor,
              focusedBorderColor = myFocusColor,
              layoutHook = myLayout,
              startupHook = myStartupHook
            }
            `additionalKeysP` myKeys