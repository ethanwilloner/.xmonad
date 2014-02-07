import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Run (safeSpawn)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.Environment (getEnvironment)
import XMonad.Util.EZConfig
import XMonad.Layout.Minimize
import XMonad.Hooks.Minimize
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Grid

mateConfig = desktopConfig
    { terminal = "mate-terminal"
    }

mateRun :: X ()
mateRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    mate_panel <- getAtom "_MATE_PANEL_ACTION"
    panel_run <- getAtom "_MATE_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw mate_panel 32 panel_run 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False



main = do
    xmonad $ mateConfig
                { modMask = mod4Mask
		 --, keys = myKeys
                 , borderWidth = 2
		 , normalBorderColor = "#8b9397"--"#ffffff"
                 , focusedBorderColor = "#0D5E9F"--"#20b2aa"   --"#7FBC71"
                } --`additionalKeysP` myKeys

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- close focused window 
    , ((modMask .|. shiftMask, xK_k     ), kill)

     -- Rotate through the available layout algorithms
    --, ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_slash ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_n     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_e     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_n     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_e     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask .|. shiftMask, xK_z     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_z     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_period), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    --, ((modMask              , xK_a     ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    --, ((modMask .|. shiftMask, xK_a     ), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    I\\\|

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    --[((m .|. modMask, k), windows $ f i)
    --    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- 

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    --[((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_u, xK_y, xK_semicolon] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


