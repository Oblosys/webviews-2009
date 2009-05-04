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

mkWebView :: (Presentable v, Storeable v, Initial v, Show v, Eq v, Data v) =>
             ViewId -> (User -> Database -> ViewMap -> ViewId -> v) -> User -> Database -> ViewMap -> WebView
mkWebView vid wvcnstr user db viewMap = loadView user db viewMap $
  WebView vid noId noId wvcnstr initial

loadView user db viewMap (WebView vid si i f _) = (WebView vid si i f (f user db viewMap vid))



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
  in mkSpan ("input"++show i) $  
       form![ thestyle "display: inline", strAttr "onSubmit" $ "textFieldChanged('"++show i++"');return false"] $
         inputField ! [identifier (show i), strAttr "VALUE" str
                      --, strAttr "onChange" $ "textFieldChanged('"++show i++"')"
                      , strAttr "onFocus" $ "elementGotFocus('"++show i++"')"
                      , strAttr "onBlur" $ "textFieldChanged('"++show i++"')"
                      ]

-- seems like this one could be in Present
presentButton :: Button -> Html
presentButton (Button (Id i) txt enabled _) = mkSpan ("input"++show i) $ 
   primHtml $ "<button " ++ (if enabled then "" else "disabled ") ++
                      "onclick=\"queueCommand('ButtonC "++show i++"')\" "++
                      "onfocus=\"elementGotFocus('"++show i++"')\">"++txt++"</button>"
-- TODO: text should be escaped

presentRadioBox :: RadioView -> Html
presentRadioBox (RadioView (Id i) items sel enabled) = 
  mkDiv ("input"++show i) $ radioBox (show i) items sel enabled
-- id is unique


--radioBox :: String -> [String] -> Int -> Html
radioBox id items selectedIx enabled =
  [ radio id (show i) ! ( [ strAttr "id" eltId 
                          , strAttr "onChange" ("queueCommand('SetC "++id++" %22"++show i++"%22')") 
                          , strAttr "onFocus" ("elementGotFocus('"++eltId++"')")
                          ]
                          ++ (if enabled && i == selectedIx then [strAttr "checked" ""] else []) 
                          ++ (if not enabled then [strAttr "disabled" ""] else [])) 
                          +++ item +++ br 
  | (i, item) <- zip [0..] items 
  , let eltId = "radio"++id++"button"++show i ]

instance Presentable WebView where
  present (WebView _ (Id stubId) _ _ _) = mkSpan (show stubId) << "ViewStub"
  
instance Presentable (Widget x) where
  present (Widget _ (Id stubId) _ _) = mkSpan (show stubId) << "WidgetStub"


-- todo button text and radio text needs to go into view
instance Presentable AnyWidget where                          
  present (RadioViewWidget rv) = presentRadioBox rv 
  present (EStringWidget es) = presentTextField es 
  present (ButtonWidget b) = presentButton b 



