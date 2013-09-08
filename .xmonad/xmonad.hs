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
import qualified XMonad.StackSet as SS
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace ( onWorkspace
                                  , PerWorkspace
                                  )
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (runProcessWithInput)
import Data.Monoid (Endo)
import System.IO ()
import qualified Data.Map as M ( Map
                               , fromList
                               , union
                               )
import qualified XMonad.StackSet as W ( greedyView
                                      , shift
                                      )
import Graphics.X11.ExtraTypes.XF86
import Data.Char (isDigit)
import XMonad.Util.Run (safeSpawn)

baseColor :: String
baseColor = "#000000"

browser :: String
browser = "chromium"

decreaseBrightness :: String
decreaseBrightness =  "/home/dries/bin/brightness.sh -"

decreaseVolume :: String
decreaseVolume = "amixer -c 0 set Master 3dB-"

font :: String
font = "xft:Ubuntu Mono-B:pixelsize=15"

frillColor :: String
frillColor = "#DC6741"

googleDocs :: String
googleDocs = browser ++ " https://docs.google.com"

increaseBrightness :: String
increaseBrightness =  "/home/dries/bin/brightness.sh +"

increaseVolume :: String
increaseVolume = "amixer -c 0 set Master 3dB+"

myTerminal :: String
myTerminal = "urxvt"

shutdown :: String
shutdown = "sudo shutdown -hP now"

songPlayer :: String
songPlayer = "/home/dries/bin/playSong.sh"

toggleMute :: String
toggleMute = "/home/dries/bin/toggleMute.sh"

toledo :: String
toledo = browser ++ " https://cygnus.cc.kuleuven.be/webapps/asso-toledo-bb_bb60/nosession/login.jsp?config=a"

unfocusedBorderColor :: String
unfocusedBorderColor = "gray"

volumeSetter :: String
volumeSetter = "/home/dries/bin/setMPDVolume.sh"

weather :: String
weather = browser ++ " http://www.weatherlink.com/user/beisbroek/index.php?view=main&headers=0"

modm :: KeyMask
modm = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 3
 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

powerButton :: KeySym
powerButton = xF86XK_PowerOff

azertyKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
azertyKeys conf@(XConfig { }) = M.fromList $
    ((modm, xK_semicolon), sendMessage (IncMasterN (-1))) 
    :
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) [0x26,0xe9,0x22,0x27,0x28,0xa7,0xe8,0x21,0xe7,0xe0],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myWorkspaces :: [String]
myWorkspaces = ["1:Terminal","2:Programming","3:Video","4:Chat","5:Web","6:Mail","7:Documents","8:Pictures","9","0:Music"]

gridColorizer :: a -> Bool -> X (String, String)
gridColorizer _ True = return (frillColor, "white")
gridColorizer _ False = return (baseColor, "white")

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = 
    [ ((modm,               xK_Down                 ), spawn decreaseVolume)    
    , ((modm,               xK_Up                   ), spawn increaseVolume)
    , ((0,                  0x1008ff11              ), spawn decreaseVolume)    
    , ((0,                  0x1008ff13              ), spawn increaseVolume)
    , ((0,               xF86XK_MonBrightnessUp), spawn "sudo /home/dries/bin/brightness_up.sh")
    , ((0,               xF86XK_MonBrightnessDown), spawn "sudo /home/dries/bin/brightness_down.sh")
    -- , ((0,                  xF86XK_MonBrightnessDown), spawn decreaseBrightness)
    -- , ((0,                  xF86XK_MonBrightnessUp  ), spawn increaseBrightness)
    , ((0,                  xF86XK_AudioMute        ), spawn toggleMute)
    , ((0,                  xF86XK_HomePage         ), spawn browser)
    , ((0,                  powerButton             ), spawn shutdown) 
    , ((modm,               xK_Return               ), sendMessage $ JumpToLayout "Full")   
    , ((modm,               xK_F2                   ), spawn "emacs")  
    , ((modm,               xK_F3                   ), spawn "sylpheed")
    , ((modm,               xK_F4                   ), spawn "urxvt -title weechat-curses -e weechat-curses")    -- launch weechat-curses
    , ((modm,               xK_F5                   ), spawn browser)    
    , ((modm,               xK_F6                   ), spawn toledo)    
    , ((modm,               xK_F7                   ), spawn googleDocs)
    , ((modm,               xK_F12                  ), spawn weather)  
    , ((0,                  xK_Print                ), spawn "import -window root -quality 100 screenshot.jpg")-- print screen
    , ((0,                  xF86XK_AudioPlay        ), spawn "mpc toggle")
    , ((0,                  xF86XK_AudioPrev        ), spawn "mpc prev")
    , ((0,                  xF86XK_AudioNext        ), spawn "mpc next")
    , ((0,                  xF86XK_AudioStop        ), spawn "mpc stop")
    , ((modm,               xF86XK_AudioMute        ), spawn toggleMute)
    , ((modm,               xK_a                    ), spawn "mpc play")    -- start  
    , ((modm,               xK_z                    ), spawn "mpc stop")    -- stop
    , ((modm .|. shiftMask, xK_t                    ), spawn "mpc toggle")  --toggle
    , ((modm,               xK_f                    ), spawn "mpc next")    -- next
    , ((modm,               xK_b                    ), spawn "mpc prev")    -- prev
    , ((modm .|. shiftMask, xK_a                    ), spawn "mpc seek 0")  -- return to beginning of song
    , ((modm,               xK_p                    ), songPrompt)
    , ((modm,               xK_s                    ), goToSelected defaultGSConfig { gs_navigate = myNavigation })
    -- don't use if caps lock standard behaviour isn't disabled in .xinitrc
    --, ((0,                  xK_Meta_L               ), goToSelected defaultGSConfig { gs_navigate = myNavigation })
    , ((modm,               xK_Menu                 ), menu personalizedGSConfig) -- 0 instead of modm
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

menu :: GSConfig (X ()) -> X ()
menu configuration = runProcessWithInput "date" ["+%a %d %b %H:%M"] "" >>= 
                     (\ xs -> runSelectedAction configuration ((init xs, spawn ""):menuItems))

menuItems :: [(String, X ())]
menuItems = [ ("internet"   , spawn browser)
            , ("scheme"     , spawn "drracket")
            , ("emacs"      , spawn "emacs")
            , ("processes"  , spawn "urxvt -title top -e top")  
            , ("music"      , spawn "urxvt -title ncmpcpp -e ncmpcpp")
            , ("chat"       , spawn "urxvt -title weechat-curses -e weechat-curses")  
            , ("mail"       , spawn "sylpheed")  
            , ("weather"    , spawn weather)
            , ("google docs", spawn googleDocs)
            ]

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
                        , layoutHook         = {- smartBorders $ -} avoidStruts myLayout
                        , terminal           = myTerminal
                        , normalBorderColor  = unfocusedBorderColor
                        , focusedBorderColor = frillColor
                        , startupHook        = setWMName "LG3D"
                        , workspaces         = myWorkspaces
                        , mouseBindings      = myMouseBindings         
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
                          , className =? "Sylpheed"       --> doShift "6:Mail"
                          , className =? "Zathura"        --> doShift "7:Documents"
                          , className =? "Evince"         --> doShift "7:Documents"
                          , className =? "llpp"           --> doShift "7:Documents"
                          , className =? "feh"            --> doShift "8:Pictures"
                          ]
               <+> manageDocks

myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings ( XConfig { } ) = M.fromList
     [ ((modm, button1), floatMoveWindow)
     , ((0, button2), mouseGesture gestures) -- 0 instead of modm
     , ((modm, button3), floatResizeWindow)
     ]
    where
        floatMoveWindow = apply mouseMoveWindow
        floatResizeWindow = apply mouseResizeWindow
        apply modifyWindow w = focus w >> modifyWindow w >> windows SS.shiftMaster
        gestures = M.fromList
            [ ([],  \_ -> goToSelected defaultGSConfig { gs_navigate = myNavigation })
            , ([L], \_ -> spawn myTerminal)
            , ([R], \_ -> runSelectedAction personalizedGSConfig musicItems)
            , ([U], \_ -> menu personalizedGSConfig)
            , ([D], \_ -> kill)
            , ([R,D,L,U,D,R,U,L], \_ -> spawn shutdown)
            ]
