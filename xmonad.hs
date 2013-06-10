import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Run (safeSpawn)
import qualified Data.Map as M
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
                 , borderWidth = 1
		 , normalBorderColor = "#ffffff"
                 , focusedBorderColor = "#20b2aa"   --"#7FBC71"
                } --`additionalKeysP` myKeys

--myKeys conf@(XConfig {XMonad.modMask = modm}) = 
--	[
--	((mod4Mask, xK_z), withFocused minimizeWindow)
  --      , ((modm .|. shiftMask, xK_z), sendMessage RestoreNextMinimizedWin)
--	]
