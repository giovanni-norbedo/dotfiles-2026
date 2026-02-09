import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.ManageDocks (docks, avoidStruts, manageDocks)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe)

-- Layouts
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile

-- Keybindings
import XMonad.Util.EZConfig (additionalKeysP)

-- Variables
myTerminal      = "kitty"
myBrowser       = "firefox"
myEditor        = "codium"
myLauncher      = "rofi -show drun -drun-match-fields name"
myModMask       = mod4Mask -- Tasto Super

-- Colors
myNormColor     = "#f8f8f2"
myFocusColor    = "#bd93f9"
myBorderWidth   = 2

-- Workspaces
myWorkspaces    = ["1", "2", "3", "4", "5", "6", "7", "8"]

-- Layouts Definition
-- Definisco il layout che mancava. Uso ResizableTall per far funzionare i tasti di ridimensionamento verticale.
myLayout = avoidStruts $ smartBorders $ mySpacing $ tiled ||| Full
  where
    tiled   = ResizableTall nmaster delta ratio []
    nmaster = 1      -- Numero predefinito di finestre nella master pane
    ratio   = 1/2    -- Proporzione predefinita dello schermo occupata dalla master pane
    delta   = 3/100  -- Percentuale di ridimensionamento
    -- Configurazione dello spazio tra le finestre
    mySpacing = spacingRaw False             -- Smart border (non applicare spazio se c'è una sola finestra? False = applica sempre)
                           (Border 5 5 5 5)  -- Bordi schermo
                           True              -- Abilita bordi schermo
                           (Border 5 5 5 5)  -- Bordi finestre
                           True              -- Abilita bordi finestre

-- Startup Hook
-- Definisco lo startup hook che mancava.
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "picom &"                -- Compositor (trasparenze/ombre)
    spawnOnce "nm-applet &"            -- Icona network manager
    spawnOnce "nitrogen --restore &"   -- Wallpaper (esempio)
    -- Aggiungi qui altri comandi di avvio

-- Keybindings
myKeys :: [(String, X ())]
myKeys =
    -- Applications
    [ ("M-<Return>", spawn myTerminal)              -- Terminal
    , ("M-S-<Return>", spawn "warp-terminal")       -- Warp Terminal
    , ("M-<Space>", spawn myLauncher)               -- Rofi
    , ("M-c", spawn myEditor)                       -- Codium
    , ("M-i", spawn myBrowser)                      -- Firefox
    , ("M-o", spawn "obsidian")                     -- Obsidian
    , ("M-n", spawn "thunar")                       -- Thunar
   
    -- System
    , ("M-<Escape>", spawn "xmonad --recompile; xmonad --restart") -- Reload config
    , ("M-S-q", io exitSuccess)                     -- Quit Xmonad
    , ("M-w", kill)                                 -- Close window
   
    -- Layouts & Focus
    , ("M-m", sendMessage NextLayout)               -- Toggle Layout (Tile/Full)
    , ("M-S-d", sendMessage (IncMasterN 1))         -- Increase windows in master
    , ("M-S-a", sendMessage (IncMasterN (-1)))      -- Decrease windows in master
    , ("M-t", withFocused $ windows . W.sink)       -- Force floating to tile
   
    -- Focus movement (Vim style)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-h", sendMessage Shrink)                   -- Resize Master (Shrink)
    , ("M-l", sendMessage Expand)                   -- Resize Master (Expand)
   
    -- Window swapping
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-m", windows W.swapMaster)               -- Swap current with master

    -- Resizing (ResizableTall required for vertical resize)
    , ("M-A-j", sendMessage MirrorShrink)
    , ("M-A-k", sendMessage MirrorExpand)

    -- Power & Session
    -- NOTA: Assicurati che sudo non chieda la password per questi comandi nel file sudoers,
    -- altrimenti non funzioneranno lanciati da qui.
    , ("M-C-p", spawn "sudo poweroff")
    , ("M-C-r", spawn "sudo reboot")
    , ("M-C-s", spawn "slock")                      -- Lock screen
   
    -- Hardware Keys
    , ("<XF86MonBrightnessUp>", spawn "sudo brightnessctl s +10%")
    , ("<XF86MonBrightnessDown>", spawn "sudo brightnessctl s 10%-")
    , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5")
    , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5")
    , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")

    -- Polybar toggle
    , ("M-C-m", spawn "polybar-msg cmd toggle")
    ]


-- Main Config
main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ docks $ def
    { terminal           = myTerminal
    , focusFollowsMouse  = True
    , clickJustFocuses   = False
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , layoutHook         = myLayout
    , startupHook        = myStartupHook
    } `additionalKeysP` myKeys