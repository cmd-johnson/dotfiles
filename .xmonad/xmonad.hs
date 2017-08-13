-- | TODO:
-- - Shortcuts for taking Screenshots
-- - Explore more available layouts
-- - Add a tray bar
--    trayer --edge top --align right --widthtype pixel --width 100 --heighttype pixel --height 19 --monitor 1 --tint 0x2d2d2d --transparent true --alpha 1

import XMonad
import XMonad.Util.Run                    (safeSpawn, spawnPipe, hPutStrLn)
import XMonad.Util.EZConfig               (additionalKeysP)
import XMonad.StackSet                    (greedyView, shift)
import Data.List                          (isSuffixOf)

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.NoBorders

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers         (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops          (ewmh, fullscreenEventHook)

-- Misc
import XMonad.Prompt                      (def)
import XMonad.Layout.IndependentScreens   (countScreens)

-- Main process
main :: IO()
main = do
  -- Add one xmobar per available screen
  nScreens <- countScreens
  xmobars <- mapM (spawnPipe . xmobarCommand) [0 .. nScreens - 1]

  xmonad $ ewmh def
    { modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , handleEventHook = myEventHook
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , logHook = mapM_ (dynamicLogWithPP . (\h -> myPP { ppOutput = hPutStrLn h })) xmobars
    }
    `additionalKeysP` myKeys

-- | Creates the command line for starting xmobar on a particular screen.
xmobarCommand :: ScreenId -> String
xmobarCommand (S s) = unwords ["xmobar", "-x", show s]

-- | These keybindings override xmonad's default keybindings. They can be found
-- at https://wiki.haskell.org/Xmonad/Config_archive/Template_xmonad.hs_(0.9) .
myKeys =
  -- Volume controls
  [ ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["-M", "set", "Master", "2%+"])
  , ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["-M", "set", "Master", "2%-"])
  , ("<XF86AudioMute>",        safeSpawn "amixer" ["-M", "set", "Master", "toggle"])
  -- Toggle xmobar visibility
  , ("M-b",                    sendMessage ToggleStruts)
  , ("M-c", kill)
  -- , ("M-e",                    safeSpawn "nemo"   [])

  , ("M-<Space>",              sendMessage NextLayout)
  -- , ("M-S-<Space>",            sendMessage XMonad.Layout)
  ]
  ++
  -- Switch through/move to workspaces using M-[1..9]/M-S-[1..9]. Use 'view'
  -- instead of 'greedyView' to change the active screen instead of swapping
  -- workspaces if the target workspace is already visible on another screen.
  [ (otherModMasks ++ "M-" ++ key, action tag)
  | (tag, key) <- zip myWorkspaces $ map show [1..9::Int]
  , (otherModMasks, action) <- [ ("", windows . greedyView)
                               , ("S-", windows . shift)
                               ]
  ]

-- | Modkey (super)
myModMask = mod4Mask
-- | Terminal
myTerminal = "gnome-terminal"

-- Available workspaces. The unicode labels are taken from Font Awesome as
-- configured in '.xmobarrc', so this only works if you have Font Awesome
-- installed. Font Awesome character names are written in parenthesis.
-- See http://fontawesome.io/icons/.
myTerminalWs = "\xf120" -- | Terminal workspace (terminal).
myInternetWs = "\xf268" -- | Browser workspace (chrome).
myCommsWs    = "\xf2c6" -- | Chat workspace (telegram).
myDevWs      = "\xf121" -- | Development workspace (code).
myFileWs     = "\xf07b" -- | File browser workspace (folder).
myArtWs      = "\xf1fc" -- | Photo manipulation workspace (paint-brush).
myGenericWs  = "\xf1db" -- | Generic workspace (circle-thin).
myGamingWs   = "\xf11b" -- | Gaming workspace (gamepad).
myMediaWs    = "\xf001" -- | Music/Video workspace (music).

-- | All configured workspaces in the order they should appear in.
myWorkspaces =
  [ myTerminalWs
  , myInternetWs
  , myCommsWs
  , myDevWs
  , myFileWs
  , myArtWs
  , myGenericWs
  , myGamingWs
  , myMediaWs
  ]

-- Layouts
myLayoutHook = avoidStruts $
          spaced (smartBorders tall)
      ||| spaced (smartBorders $ GridRatio 1.78) -- about 16/9
      ||| spaced emptyBSP
      ||| noBorders Full
  where
    spaced = spacing 2
    tall = Tall 1 (5/100) (1/2)

-- Mangehooks
myManageHook = composeAll
  [ isFullscreen --> doFullFloat
  , className =? "Firefox" --> doShift myInternetWs
  , className =? "Chromium" --> doShift myInternetWs

  , className =? "Telegram" --> doShift myCommsWs
  , className =? "TeamSpeak 3" --> doShift myCommsWs
  , className =? "discord" --> doShift myCommsWs

  , className =? "Atom" --> doShift myDevWs
  , className =? "Filezilla" --> doShift myDevWs

  , className =? "Blender" --> doShift myArtWs
  , className =? "Inkscape" --> doShift myArtWs
  , className =? "krita" --> doShift myArtWs
  -- manage Gimp toolbox windows
  , className =? "Gimp" --> doShift myArtWs -- may be "Gimp" or "Gimp-2.4" instead
  , (className =? "Gimp" <&&> fmap ("tool" `isSuffixOf`) role) --> doFloat

  , className =? "libreoffice" --> doShift myGenericWs
  , className =? "libreoffice-startcenter" --> doShift myGenericWs

  , className =? "vlc" --> doShift myMediaWs

  , className =? "Steam" --> doShift myGamingWs
  , manageDocks
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

-- Event Hooks
myEventHook = docksEventHook <+> fullscreenEventHook

-- Looks
myBorderWidth = 1
myNormalBorderColor = "#6d6d6d"
myFocusedBorderColor = "#cc342b"

-- Xmobar style options
myBar = "xmobar"
myPP = xmobarPP
  { ppCurrent         = xmobarColor "#d64937" ""
  , ppHidden          = xmobarColor "#dcdcdc" ""
  , ppHiddenNoWindows = xmobarColor "#535353" ""
  , ppUrgent          = xmobarColor "#3d3d3d" ""
  , ppLayout          = xmobarColor "#dcdcdc" ""
  , ppTitle           = xmobarColor "#dcdcdc" "" . shorten 80
  , ppSep             = xmobarColor "#dcdcdc" "" "  "
  }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Keyboard shortcuts
-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
--     -- launching apps
--     [ ((modMask .|. controlMask, xK_Return), safeSpawn (XMonad.terminal conf) [])
--     , ((modMask,                 xK_p     ), safeSpawn "rofi" ["-show", "run"])
--     , ((modMask,                 xK_o     ), safeSpawn "rofi" ["-show", "window"])
--     , ((modMask .|. controlMask, xK_c     ), safeSpawn "firefox" [])
--     , ((modMask .|. controlMask, xK_b     ), safeSpawn "chromium" [])
--     , ((modMask .|. controlMask, xK_p     ), safeSpawn "pcmanfm" [])
--     -- launching cli apps
--     , ((modMask .|. controlMask, xK_n     ), safeSpawn "urxvtc" ["-name", "ncmpcpp", "-e", "ncmpcpp"])
--     , ((modMask .|. controlMask, xK_f     ), safeSpawn "urxvtc" ["-name", "ranger", "-e", "ranger"])
--     , ((modMask .|. controlMask, xK_i     ), safeSpawn "urxvtc" ["-name", "irssi", "-e", "irssi"])
--     , ((modMask .|. controlMask, xK_r     ), safeSpawn "urxvtc" ["-name", "rainbowstream", "-e", "rainbowstream"])
--     , ((modMask .|. controlMask, xK_v     ), safeSpawn "urxvtc" ["-name", "vim", "-e", "nvim"])
--     , ((modMask .|. controlMask, xK_m     ), safeSpawn "urxvtc" ["-name", "mutt", "-e", "mutt"])
--     -- Kill windows
--     , ((modMask .|. controlMask, xK_w     ), kill)
--     -- lock screen
--     , ((modMask .|. controlMask, xK_Delete), safeSpawn "rmlock.sh" [])
--     -- screenshot
--     , ((0, xK_Print                       ), safeSpawn "scrot" [])
--     -- multimedia
--     , ((0, xF86XK_AudioRaiseVolume      ), safeSpawn "pamixer" ["-i", "5"])
--     , ((0, xF86XK_AudioLowerVolume      ), safeSpawn "pamixer" ["-d", "5"])
--     , ((modMask,                 xK_Down), safeSpawn "mpc" ["toggle"])
--     , ((modMask,                 xK_Up),   safeSpawn "mpc" ["stop"])
--     , ((modMask,                 xK_Left), safeSpawn "mpc" ["prev"])
--     , ((modMask,                 xK_Right), safeSpawn "mpc" ["next"])
--     -- layouts
--     , ((modMask,               xK_space ), sendMessage NextLayout)
--     , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
--
--     -- floating layer stuff
--     , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
--
--     -- refresh
--     , ((modMask,               xK_n     ), refresh)
--
--     -- focus
--     , ((modMask,               xK_Tab   ), windows W.focusDown)
--     , ((modMask,               xK_j     ), windows W.focusDown)
--     , ((modMask,               xK_k     ), windows W.focusUp)
--     , ((modMask,               xK_m     ), windows W.focusMaster)
--
--     -- swapping
--     , ((modMask,               xK_Return), windows W.shiftMaster)
--     , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
--     , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
--
--     -- increase or decrease number of windows in the master area
--     , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
--     , ((modMask              , xK_semicolon), sendMessage (IncMasterN (-1)))
--
--     -- resizing
--     , ((modMask,               xK_h     ), sendMessage Shrink)
--     , ((modMask,               xK_l     ), sendMessage Expand)
--     -- quit, or restart
--     , ((modMask .|. shiftMask, xK_Escape  ), io (exitWith ExitSuccess))
--     , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
--     ]
--     ++
--     [((m .|. modMask, k), windows $ f i)
--         | (i, k) <- zip (workspaces conf)[ xK_ampersand
--                                          , xK_eacute
--                                          , xK_quotedbl
--                                          , xK_apostrophe
--                                          , xK_parenleft
--                                          , xK_section -- 6 0xa7
--                                          , xK_egrave
--                                          , xK_exclam  -- 8 0x21
--                                          , xK_ccedilla
--                                          , xK_agrave
--                                          , xK_parenright
--                                          ] ,
--           (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
