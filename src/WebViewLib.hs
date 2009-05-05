module WebViewLib where

import Control.Monad.Trans
import Data.List
import Text.Html hiding (image)
import qualified Text.Html as Html
import Data.Generics
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Data.Tree
import Debug.Trace
import Types
import Database
import Generics
import HtmlLib


-- IDEA: use phantom types to enforce type structure on views
--       GADTs?
-- TODO: figure out load stuff
--       use a monad for auto handling listeners to database


-- When loading from different point than root, make sure Id's are not messed up

instance Storeable WebView where
  save (WebView _ _ _ _ v) =
    let topLevelWebViews = getTopLevelWebViews v
    in  foldl (.) id $ save v : map save topLevelWebViews

-- first save self, then children
-- TODO: does this order matter?

-- should be with TH stuff for Database, like DatabaseUtils or something
instance Initial VisitId where initial = VisitId (-1)

instance Initial PigId where initial = PigId (-1)



mkViewEdit i f = 
  ViewEdit i $ \(WebView vi si i lv v) -> WebView vi si i lv $ applyIfCorrectType f v


applyIfCorrectType :: (Typeable y, Typeable x) => (y -> y) -> x -> x
applyIfCorrectType f x = case cast f of 
                           Just fx -> fx x
                           Nothing -> x

-- the view matching on load can be done explicitly, following structure and checking ids, or
-- maybe automatically, based on id. Maybe extra state can be in a separate data structure even,
-- like in Proxima

mkWebView' :: (Presentable v, Storeable v, Initial v, Show v, Eq v, Data v) =>
             (User -> Database -> ViewMap -> Int -> ViewId -> (v, Int)) -> 
             User -> Database -> ViewMap -> WVMonad WebView
mkWebView' wvcnstr user db viewMap = 
  WV $ loadView user db viewMap (WebView noViewId noId noId wvcnstr initial)

loadView :: User -> Database -> ViewMap -> WebView -> Int -> (WebView, Int)
loadView user db viewMap (WebView vid si i f _) viewIdCounter =
  let vid = ViewId viewIdCounter
      (view,viewIdCounter') = f user db viewMap (viewIdCounter+1) vid
  in  (WebView vid si i f view,viewIdCounter')

mkWebView f = 
  mkWebView' $ \user db viewMap vidC vid -> runWV vidC $ f user db viewMap vid 


{-

Everything seems to work:
not updating the unchanged controls and keeping id's unique causes edit events survive update
They do seem to lose focus though, but since we know what was edited, we can easily restore the focus after
the update
-}


-- textfields are in forms, that causes registering text field updates on pressing enter
-- (or Done) on the iPhone.

presentTextField :: EString -> Html
presentTextField (EString (Id i) hidden str) = 
  let inputField = if hidden then password "" else textfield ""
  in form![ thestyle "display: inline", strAttr "onSubmit" $ "textFieldChanged('"++show i++"');return false"] $
       inputField ! [identifier (show i), strAttr "value" str
                    --, strAttr "onChange" $ "textFieldChanged('"++show i++"')"
                    , strAttr "onFocus" $ "elementGotFocus('"++show i++"')"
                    , strAttr "onBlur" $ "textFieldChanged('"++show i++"')"
                    ]

-- seems like this one could be in Present
presentButton :: Button -> Html
presentButton (Button (Id i) txt enabled _) = 
   primHtml $ "<button " ++ (if enabled then "" else "disabled ") ++
                            "onclick=\"queueCommand('ButtonC "++show i++"')\" "++
                            "onfocus=\"elementGotFocus('"++show i++"')\">"++txt++"</button>"
-- TODO: text should be escaped

presentRadioBox :: RadioView -> Html
presentRadioBox (RadioView (Id id) items selectedIx enabled) = thespan << 
  [ radio (show id) (show i) ! ( [ strAttr "id" eltId 
                          , strAttr "onChange" ("queueCommand('SetC "++show id++" %22"++show i++"%22')") 
                          , strAttr "onFocus" ("elementGotFocus('"++eltId++"')")
                          ]
                          ++ (if enabled && i == selectedIx then [strAttr "checked" ""] else []) 
                          ++ (if not enabled then [strAttr "disabled" ""] else [])) 
                          +++ item +++ br 
  | (i, item) <- zip [0..] items 
  , let eltId = "radio"++show id++"button"++show i ] -- these must be unique for setting focus

instance Presentable WebView where
  present (WebView _ (Id stubId) _ _ _) = mkSpan (show stubId) << "ViewStub"
  
instance Presentable (Widget x) where
  present (Widget _ (Id stubId) _ _) = mkSpan (show stubId) << "WidgetStub"


-- todo button text and radio text needs to go into view
instance Presentable AnyWidget where                          
  present (RadioViewWidget rv) = presentRadioBox rv 
  present (EStringWidget es) = presentTextField es 
  present (ButtonWidget b) = presentButton b 






-- Login -----------------------------------------------------------------------  

data LoginView = LoginView (Widget EString) (Widget EString) (Widget Button) 
  deriving (Eq, Show, Typeable, Data)

mkLoginView = mkWebView $
  \user db viewMap vid ->
   do { let (LoginView name password b) = getOldView vid viewMap
      ; nameT <- mkTextField $ getStrVal name 
      ; passwordT <- mkPasswordField $ getStrVal password 
      ; loginB <- mkButton "Login" True $ 
                    AuthenticateEdit (widgetGetViewRef nameT) (widgetGetViewRef passwordT)
      ; return $ LoginView nameT passwordT loginB
      }

instance Storeable LoginView where save _ = id
                                   
instance Presentable LoginView where
  present (LoginView name password loginbutton) = 
    boxed $ simpleTable [] [] [ [ stringToHtml "Login:", present name]
                              , [ stringToHtml "Password:", present password]
                              , [ present loginbutton ]
                              ]
            
instance Initial LoginView where initial = LoginView initial initial initial



-- Logout ----------------------------------------------------------------------  

data LogoutView = LogoutView (Widget Button) deriving (Eq, Show, Typeable, Data)

mkLogoutView = mkWebView $
  \(Just (l,_)) db viewMap vid -> 
   do { logoutB <- mkButton ("Logout " ++  l) True LogoutEdit
      ; return $ LogoutView logoutB
      }
instance Storeable LogoutView where save _ = id
                                   
instance Presentable LogoutView where
  present (LogoutView logoutbutton) = 
    present logoutbutton
            
instance Initial LogoutView where initial = LogoutView initial