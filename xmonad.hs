import XMonad hiding ((|||))
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Layout.Spiral
import XMonad.Layout.LayoutCombinators
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer ( updatePointer
                                    , PointerPosition(Relative)
                                    )
import XMonad.Hooks.SetWMName
import XMonad.Actions.MouseGestures
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace ( onWorkspace
                                  , PerWorkspace
                                  )
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import Data.Monoid (Endo)
import System.IO ()
import qualified Data.Map as M ( Map
                               , fromList
                               , union
                               )
import qualified XMonad.StackSet as W ( greedyView
                                      , shift
                                      )
--import Graphics.X11.ExtraTypes.XF86
import Data.Char (isDigit)


baseColor :: String
baseColor = "#000000"

font :: String
font = "xft:Ubuntu Mono-B:pixelsize=15"

frillColor :: String
frillColor = "#DC6741"

myTerminal :: String
myTerminal = "urxvt"

songPlayer :: String
songPlayer = "/home/dries/bin/playSong.sh"

unfocusedBorderColor :: String
unfocusedBorderColor = "gray"

volumeSetter :: String
volumeSetter = "/home/dries/bin/setMPDVolume.sh"

modm :: KeyMask
modm = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 3
 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

azertyKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
azertyKeys conf@(XConfig { }) = M.fromList $
    ((modm, xK_semicolon), sendMessage (IncMasterN (-1))) 
    :
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) [0x26,0xe9,0x22,0x27,0x28,0xa7,0xe8,0x21,0xe7,0xe0],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myWorkspaces :: [String]
myWorkspaces = ["1:Terminal","2:Programming","3:Video","4:Chat","5:Web","6:Mail","7:Documents","8:Pictures","9:System","0:Music"]

gridColorizer :: a -> Bool -> X (String, String)
gridColorizer _ True = return (frillColor, "white")
gridColorizer _ False = return (baseColor, "white")

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = 
    [ ((modm,               xK_Return               ), sendMessage $ JumpToLayout "Full")   
    , ((modm,               xK_p                    ), songPrompt)
    ]

songPrompt :: X ()
songPrompt = inputPrompt defaultXPConfig { bgColor = baseColor 
                                         , fgColor = "#FFFFFF"
                                         , promptBorderWidth = myBorderWidth
                                         , borderColor = frillColor
                                         , XMonad.Prompt.font = Main.font
                                         , height = 30
                                         }
                         "Song" ?+ (\ x ->  if (all isDigit x) then setVolume x else playSong x)


playSong :: String -> X ()
playSong song = safeSpawn songPlayer [song]

setVolume :: String -> X ()
setVolume volume = spawn (volumeSetter ++ " " ++ volume)

musicItems :: [(String, X ())]
musicItems = [ ("play"   , spawn "mpc play")
             , ("stop"     , spawn "mpc stop")
             , ("next song"      , spawn "mpc next")
             , ("toggle"      , spawn "mpc toggle")
             , ("previous song"  , spawn "mpc prev")  
             , ("choose song", songList)
             ]

songList :: X ()
songList = runProcessWithInput "mpc" ["-f","%title%","playlist"] "" >>= 
           (\ xs -> runSelectedAction personalizedGSConfig (zip (lines xs) 
                                                                (map (\ x -> spawn ("mpc play " ++ (show x)))
                                                                     ([1..] :: [Integer]))))

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
               where navKeyMap = M.fromList 
                       [ ((0,xK_Escape)         , cancel)
                       , ((0,xK_Return)         , select)
                       , ((shiftMask, xK_slash) , substringSearch   myNavigation)
                       , ((0,xK_Left)           , move   (-1,0)  >> myNavigation)
                       , ((0,xK_h)              , move   (-1,0)  >> myNavigation)
                       , ((0,xK_Right)          , move   (1,0)   >> myNavigation)
                       , ((0,xK_l)              , move   (1,0)   >> myNavigation)
                       , ((0,xK_Down)           , move   (0,1)   >> myNavigation)
                       , ((0,xK_j)              , move   (0,1)   >> myNavigation)
                       , ((0,xK_Up)             , move   (0,-1)  >> myNavigation)
                       , ((0,xK_y)              , move   (-1,-1) >> myNavigation)
                       , ((0,xK_i)              , move   (1,-1)  >> myNavigation)
                       , ((0,xK_n)              , move   (-1,1)  >> myNavigation)
                       , ((0,xK_m)              , move   (1,-1)  >> myNavigation)
                       , ((0,xK_space)          , setPos (0,0)   >> myNavigation)
                       ]

navDefaultHandler :: b -> TwoD a (Maybe a)
navDefaultHandler = const myNavigation

personalizedGSConfig :: GSConfig a
personalizedGSConfig = (buildDefaultGSConfig gridColorizer) { gs_navigate = myNavigation }

myLayout :: PerWorkspace (NewSelect SpiralWithDir Full) (NewSelect Tall (NewSelect (Mirror Tall) (NewSelect Full Grid))) a
myLayout = onWorkspace "1:Terminal"  (spiral (toRational (2/(1+sqrt(5)::Double))) ||| Full) (tiled ||| Mirror tiled ||| Full ||| Grid)
  where
    tiled   = Tall nmaster delta ratio    -- default tiling algorithm partitions the screen into two panes
    nmaster = 1                           -- The default number of windows in the master pane
    ratio   = 1/2                         -- Default proportion of screen occupied by master pane
    delta   = 3/100                       -- Percent of screen to increment by when resizing panes

main :: IO ()
main = xmonad $ defaultConfig { keys = \c -> azertyKeys c `M.union` keys defaultConfig c } 
                        { modMask            = modm
                        , logHook            = updatePointer (Relative 0.95 0.95)
                        , manageHook         = myManageHook
                        , layoutHook         = smartBorders $ avoidStruts myLayout
                        , terminal           = myTerminal
                        , normalBorderColor  = unfocusedBorderColor
                        , focusedBorderColor = frillColor
                        , startupHook        = setWMName "LG3D"
                        , workspaces         = myWorkspaces
--                        , mouseBindings      = myMouseBindings         
                        , focusFollowsMouse  = myFocusFollowsMouse
                        , borderWidth        = myBorderWidth
                        } `additionalKeys`myKeys
               
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll [ title     =? "ncmpcpp"        --> doShift "0:Music"
                          , title     =? myTerminal       --> doShift "1:Terminal"
                          , title     =? "eshell"         --> doShift "1:Terminal"
                          , className =? "Emacs"          --> doShift "2:Programming"
                          , className =? "MPlayer"        --> doShift "3:Video"
                          , title     =? "weechat-curses" --> doShift "4:Chat" 
                          , className =? "Chromium"       --> doShift "5:Web"
                          , className =? ".dwb-wrapped"       --> doShift "5:Web"
                          , title     =? "mutt"           --> doShift "6:Mail"
                          , className =? "Zathura"        --> doShift "7:Documents"
                          , className =? "Evince"         --> doShift "7:Documents"
                          , className =? "llpp"           --> doShift "7:Documents"
                          , className =? "MuPDF"          --> doShift "7:Documents"
                          , className =? "feh"            --> doShift "8:Pictures"
                          , title     =? "htop"           --> doShift "9:System"
                          ]
               <+> manageDocks
