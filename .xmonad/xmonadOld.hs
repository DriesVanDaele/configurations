import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer ( updatePointer
                                    , PointerPosition(Relative)
                                    )
import XMonad.Actions.MouseGestures
import qualified XMonad.StackSet as SS
import XMonad.Hooks.EwmhDesktops ( fullscreenEventHook -- handles chrome fullscreen  
                                 , ewmh 
                                 )    
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Magnifier ( Magnifier
                               , magnifier
                               )
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace ( onWorkspace
                                  , PerWorkspace
                                  )
import XMonad.Util.EZConfig (additionalKeys)
import Data.Monoid (Endo)
import System.IO ()
import qualified Data.Map as M ( Map
                               , fromList
                               , union
                               )
import qualified XMonad.StackSet as W ( greedyView
                                      , shift
                                      )
unfocusedBorderColor :: String
unfocusedBorderColor = "gray"

baseColor :: String
baseColor = "#000000" --160549

frillColor :: String
frillColor = "#EC0549"

font :: String
font = "-*-*-*-*-*-*-*-*-*-*-*-*-*-*" --use xfontsel to generate a valid string

decreaseVolume :: String
decreaseVolume = "amixer -c 0 set Master 3dB-"

googleDocs :: String
googleDocs = "chromium --app=https://docs.google.com"

increaseVolume :: String
increaseVolume = "amixer -c 0 set Master 3dB+"

myTerminal :: String
myTerminal = "urxvt"

shutdown :: String
shutdown = "sudo shutdown -hP now"

toggleMute :: String
toggleMute = "toggleMute.sh"

weather :: String
weather = "chromium --app=http://www.weatherlink.com/user/beisbroek/index.php?view=main&amp;headers=0"

toledo :: String
toledo = "chromium --new-window https://cygnus.cc.kuleuven.be/webapps/asso-toledo-bb_bb60/nosession/login.jsp?config=a"

dmenu :: String
dmenu = "export DMENU_OPTIONS='-b -nb '" ++ baseColor ++ "' -sb '" ++ frillColor ++ "' -fn '" ++ font ++ "' ' && dmenu-launch"

modm :: KeyMask
modm = mod4Mask -- mod3Mask ("right alt") mod4Mask ("super key")

myBorderWidth :: Dimension
myBorderWidth = 3
 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

powerButton :: KeySym
powerButton = 0x1008ff2A

azertyKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
azertyKeys conf@(XConfig { }) = M.fromList $
    ((modm, xK_semicolon), sendMessage (IncMasterN (-1))) 
    :
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) [0x26,0xe9,0x22,0x27,0x28,0xa7,0xe8,0x21,0xe7,0xe0],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myWorkspaces :: [String]
myWorkspaces = ["1:Terminal","2:Programming","3:Video","4:Chat","5:Web","6:Mail","7:Documents","8","9","0:Music"]

gridColorizer :: a -> Bool -> X (String, String)
gridColorizer _ True = return (frillColor, "white")
gridColorizer _ False = return (baseColor, "white")

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = 
    [ ((0,                  0x1008ff12 ), spawn toggleMute)
    , ((0,                  0x1008ff11 ), spawn decreaseVolume)
    , ((modm,               xK_Down    ), spawn decreaseVolume)    
    , ((0,                  0x1008ff13 ), spawn increaseVolume)
    , ((modm,               xK_Up      ), spawn increaseVolume)
    , ((0,                  powerButton), spawn shutdown) 
    , ((modm,               xK_p       ), spawn dmenu)    
    , ((modm,               xK_F2      ), spawn "emacs")  
    , ((modm,               xK_F3      ), spawn "sylpheed")
    , ((modm,               xK_F4      ), spawn "urxvt -title irssi -e irssi")    -- launch irssi
    , ((modm,               xK_F5      ), spawn "chromium")    
    , ((modm,               xK_F6      ), spawn toledo)    
    , ((modm,               xK_F7      ), spawn googleDocs)
    , ((modm,               xK_F12     ), spawn weather)  
    , ((0,                  xK_Print   ), spawn "import -window root -quality 100 screenshot.jpg")    -- print screen
    , ((modm,               xK_a       ), spawn "ncmpcpp play")    -- start  
    , ((modm,               xK_z       ), spawn "ncmpcpp stop")    -- stop
    , ((modm .|. shiftMask, xK_t       ), spawn "ncmpcpp toggle")    --toggle
    , ((modm,               xK_f       ), spawn "ncmpcpp next")    -- next
    , ((modm,               xK_b       ), spawn "ncmpcpp prev")    -- prev
    , ((modm,               xK_s       ), goToSelected defaultGSConfig { gs_navigate = myNavigation })
    , ((0,                  xK_Menu    ), runSelectedAction personalizedGSConfig menuItems)
    ]

menuItems :: [(String, X ())]
menuItems = [ ("internet"   , spawn "chromium")
            , ("scheme"     , spawn "drracket")
            , ("emacs"      , spawn "emacs")
            , ("processes"  , spawn "urxvt -title top -e top")  
            , ("music"      , spawn "urxvt -title ncmpcpp -e ncmpcpp")
            , ("chat"       , spawn "urxvt -title irssi -e irssi")  
            , ("mail"       , spawn "sylpheed")  
            , ("weather"    , spawn weather)
            , ("google docs", spawn googleDocs)
            ]

ncmpcppItems :: [(String, X ())]
ncmpcppItems = [ ("play"   , spawn "ncmpcpp play")
               , ("stop"     , spawn "nmpcpp stop")
               , ("toggle"      , spawn "ncmpcpp toggle")
               , ("previous song"  , spawn "ncmpcpp prev")  
               , ("next song"      , spawn "ncmpcpp next")
               ]


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

myLayout :: PerWorkspace (ModifiedLayout Magnifier Grid) (Choose Tall (Choose (Mirror Tall) (Choose Full Grid))) Window
myLayout = onWorkspace "1:Terminal" (magnifier Grid) (tiled ||| Mirror tiled ||| Full ||| Grid)
  where
    tiled   = Tall nmaster delta ratio    -- default tiling algorithm partitions the screen into two panes
    nmaster = 1                           -- The default number of windows in the master pane
    ratio   = 1/2                         -- Default proportion of screen occupied by master pane
    delta   = 3/100                       -- Percent of screen to increment by when resizing panes

main :: IO ()
main = xmonad $ ewmh defaultConfig { keys = \c -> azertyKeys c `M.union` keys defaultConfig c } 
                        { modMask            = modm
                        , logHook            = updatePointer (Relative 0.95 0.95)
                        , manageHook         = myManageHook
                        , layoutHook         = smartBorders $ avoidStruts myLayout
                        , terminal           = myTerminal
                        , normalBorderColor  = unfocusedBorderColor
                        , focusedBorderColor = frillColor
                        , workspaces         = myWorkspaces
                        , mouseBindings      = myMouseBindings         
                        , handleEventHook    = fullscreenEventHook <+> docksEventHook
                        , focusFollowsMouse  = myFocusFollowsMouse
                        , borderWidth        = myBorderWidth
                        } `additionalKeys`myKeys
               
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll [ title     =? "ncmpcpp"  --> doShift "0:Music"
                          , title     =? myTerminal --> doShift "1:Terminal"
                          , className =? "Emacs"    --> doShift "2:Programming"
                          , className =? "MPlayer"  --> doShift "3:Video"
                          , title     =? "irssi"    --> doShift "4:Chat" 
                          , className =? "Chromium" --> doShift "5:Web"
                          , className =? "Sylpheed" --> doShift "6:Mail"
                          , className =? "Zathura"  --> doShift "7:Documents"
                          ]
               <+> manageDocks

myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings ( XConfig { } ) = M.fromList
     [ ((modm, button1), floatMoveWindow)
     , ((0, button2), mouseGesture gestures) 
     , ((modm, button3), floatResizeWindow)
     ]
    where
        floatMoveWindow = apply mouseMoveWindow
        floatResizeWindow = apply mouseResizeWindow
        apply modifyWindow w = focus w >> modifyWindow w >> windows SS.shiftMaster
        gestures = M.fromList
            [ ([],  \_ -> goToSelected defaultGSConfig { gs_navigate = myNavigation })
            , ([L], \_ -> spawn myTerminal)
            , ([R], \_ -> runSelectedAction personalizedGSConfig ncmpcppItems)
            , ([U], \_ -> runSelectedAction personalizedGSConfig menuItems)
            , ([D], \_ -> kill)
            ]


        -- floatMoveWindow = \w -> apply mouseMoveWindow 
        -- floatResizeWindow = \w -> apply mouseResizeWindow 
        -- apply modification = \w -> focus w >> modification w >> windows SS.shiftMaster
