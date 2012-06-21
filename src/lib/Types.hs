{-# OPTIONS -XExistentialQuantification -XFlexibleContexts -XTypeSynonymInstances -XFlexibleInstances -XDeriveDataTypeable -XMultiParamTypeClasses #-}
module Types where

import Data.IORef
import Data.Generics
import Data.List.Split
import Data.Char
import BlazeHtml
import Text.Show.Functions
import Data.Map (Map)
import qualified Data.Map as Map 
import Control.Monad.State
import Control.Monad.Identity
import GHC.Read (parens, readPrec, lexP)
import Text.Read.Lex (Lexeme(Ident))
import Text.ParserCombinators.ReadPrec (pfail)


-- Typeable1 instance for StateT, which comes from happstack-state-6.1.4/src/Happstack/State/Types.hs 
-- (and has been updated to use mkTyCon3 instead of the deprecated mkTycon)
instance (Typeable st, Typeable1 m) => Typeable1 (StateT st m) where
    typeOf1 x = mkTyConApp (mkTyCon3 "mtl" "Control.Monad.State.Lazy" "StateT") [typeOf (undefined :: st), typeOf1 (m x)]
        where m :: StateT st m a -> m a
              m = undefined

data Commands = Commands [Command] 
              | SyntaxError String -- for debugging post from client, replace read by Str in FromData instance
                  deriving (Eq, Show, Read)

getCommands (Commands cs) = cs
getCommands _             = []

data Command = Init       String [(String,String)] -- rootView args    http://webviews.com/#rootView/arg0/arg1/../argn
             | HashUpdate String [(String,String)] -- rootView args    http://webviews.com/#rootView/arg0/arg1/../argn
             | Refresh 
             | Test 
             | SetC ViewId String 
             | ButtonC ViewId -- ViewId
             | SubmitC ViewId -- ViewId
             | PerformEditActionC ViewId [String] 
             | ConfirmDialogOk 
               deriving (Eq, Show, Read) 


-- view id's are for identifying views and widgets with regard to incrementality
-- they remain constant over the view's/widget's life
-- for now, we assign them at mkView
newtype ViewId = ViewId [Int] deriving (Eq, Ord, Typeable, Data)

unViewId  (ViewId pth) = pth -- not defined as a field, since ViewId's are now simply showed to get javascript id's, and we don't
                             -- want them to look like ... id = "ViewID {unViewId = [..]}" 

noViewId = ViewId []

-- Html id attributes may only contain letters, digits, '_' and '-', so we can't simply show the viewId in html id attrs.
instance Show ViewId where
  show (ViewId is) = "VID_"++concatMap (\i->show i++"_") is


instance Read ViewId where
  readPrec = parens $ do { Ident ('V':'I':'D':'_':pathStr) <- lexP
                         ; case takePathElts pathStr of
                             Just path -> return $ ViewId path
                             Nothing   -> pfail
                         }
   where takePathElts :: String -> Maybe [Int] -- return path if we can parse the entire string
         takePathElts ""   = Just []
         takePathElts str = case span isDigit str of
                              (digits@(_:_), ('_': rest')) -> do { path <- takePathElts rest'
                                                                 ; return $ read digits : path
                                                                 }      
                              _                      -> Nothing                               

-- we define an AttributeValue rather than the id_ Attribute, so each application will still contain the id_ combinator (which 
-- makes it more clear that an id is set)
mkHtmlViewIdVal :: ViewId -> AttributeValue
mkHtmlViewIdVal vid = toValue $ show vid


newtype Id = Id { unId :: Int } deriving (Show, Eq, Ord, Data, Typeable)

noId = Id (-1)


-- refs are different type, because they may be part of view tree, and SYB id assignment functions 
-- should not affect them
newtype ViewIdRef = ViewIdRef [Int] deriving (Show, Eq, Ord, Data, Typeable)

newtype IdRef = IdRef Int deriving (Show, Eq, Ord, Data, Typeable)

mkViewRef (ViewId i) = ViewIdRef i

mkRef (Id i) = IdRef i

----- Widgets

data Widget w = Widget { getWidgetStubId :: Id, getWidgetId :: Id, getWidgetWidget :: w }
              deriving (Show, Typeable, Data)
                       
instance Eq (Widget w) where
  w1 == w2 = True

data AnyWidget db = LabelWidget !LabelView
                  | TextWidget !(TextView db)
                  | RadioViewWidget !RadioView 
                  | SelectViewWidget !SelectView 
                  | ButtonWidget !(Button db)
                  | JSVarWidget !JSVar -- TODO: not really a widget, but until we know what it is, or what we should call widget, it is here 
                    deriving (Eq, Show, Typeable, Data)

-- Label

-- does not have a html counterpart. It is just a div with a view id that contains a string element
data LabelView = LabelView { getLabelViewId :: ViewId, getLabelText :: String } deriving (Show, Typeable, Data)

instance Eq LabelView where
  LabelView _ t1 == LabelView _ t2 = t1 == t2
  
labelView viewId txt = Widget noId noId $ LabelView viewId txt

-- Text

-- todo rename Str stuff in Text

data TextType = TextField | PasswordField | TextArea deriving (Eq, Show, Typeable, Data)

data TextView db = TextView { getTextViewId :: ViewId, getTextType :: TextType, getStrVal' :: String 
                    , getSubmitAction :: Maybe (EditCommand db) } deriving (Show, Typeable, Data)

instance Eq (TextView db) where
  TextView _ t1 str1 _ == TextView _ t2 str2 _ = t1 == t2 && str1 == str2
  
getStrVal (Widget _ _ (TextView vi h v _)) = v

textField viewId str mSubmitAction = Widget noId noId $ TextView viewId TextField str mSubmitAction

passwordField viewId str mSubmitAction = Widget noId noId $ TextView viewId PasswordField str mSubmitAction

textArea viewId str = Widget noId noId $ TextView viewId TextArea str Nothing

strRef (Widget _ _ (TextView (ViewId i) h _ _)) = ViewIdRef i


-- RadioView and SelectView

-- We could put RadioView and SelectView in a single OptionView, since only the presentation is different,
-- but then the type in the webview data declarations would be OptionView, which is more confusing than
-- RadioView and SelectView. 

class HasSelection v where
  getSelection :: v -> Int
  setSelection :: Int -> v -> v

instance HasSelection v => HasSelection (Widget v) where
  getSelection (Widget _ _ v) = getSelection v
  setSelection s (Widget si i v) = Widget si i $ setSelection s v
 
-- RadioView

data RadioView = RadioView { getRadioViewId :: ViewId, getItems :: [String], getRadioSelection' :: Int 
                           , getRadioEnabled :: Bool 
                           } deriving (Show, Typeable, Data)
   
instance Eq RadioView where
  RadioView _ items1 int1 enabled1 == RadioView _ items2 int2 enabled2 = 
    items1 == items2 && int1 == int2 && enabled1 == enabled2

instance HasSelection RadioView where
  getSelection (RadioView i is sel _) = sel
  setSelection s (RadioView vi its _ en) = RadioView vi its s en

radioView viewId its i enabled = Widget noId noId $ RadioView viewId its i enabled

-- SelectView

data SelectView = SelectView { getSelectViewId :: ViewId, getSelectItems :: [String], getSelection' :: Int 
                             , getSelectEnabled :: Bool 
                             } deriving (Show, Typeable, Data)

instance Eq SelectView where
  SelectView _ items1 int1 enabled1 == SelectView _ items2 int2 enabled2 = 
    items1 == items2 && int1 == int2 && enabled1 == enabled2

instance HasSelection SelectView where
  getSelection (SelectView i is sel _) = sel
  setSelection s (SelectView vi its _ en) = SelectView vi its s en

selectView viewId its i enabled = Widget noId noId $ SelectView viewId its i enabled
  
-- Button

data Button db = Button { getButtonViewId :: ViewId, buttonText :: String
                        , getButtonEnabled :: Bool, getStyle :: String, getOnClick :: String 
                        , getCommand' :: EditCommand db 
                        } deriving (Show, Typeable, Data)

instance Eq (Button db) where
  Button _ txt1 enabled1 style1 onclick1 _ == Button _ txt2 enabled2 style2 onclick2 _ = txt1 == txt2 && enabled1 == enabled2 && style1 == style2 && onclick1 == onclick2

button viewId txt enabled style onclick cmd = Widget noId noId $ Button viewId txt enabled style onclick cmd

-- JSVar

data JSVar = JSVar { getJSVarViewId :: ViewId, getJSVarName :: String, getJSVarValue_ :: String } deriving (Show, Typeable, Data)

instance Eq JSVar where
  JSVar _ n1 v1 == JSVar _ n2 v2 = n1 == n2 && v1 == v2

-- renamed, so we can use jsVar for declaring a javascript variable. This constructor will probably be removed anyway
jsVar_ viewId name value = Widget noId noId $ JSVar viewId name value

getJSVarValue (Widget _ _ jsv) = getJSVarValue_ jsv


--- Editing

-- EditAction

data EditAction db = EditAction { getActionViewId :: ViewId, getCommand :: [String] -> EditCommand db 
                             } deriving (Show, Typeable, Data)

instance Eq (EditAction db) where
  EditAction _ _ == EditAction _ _ = True
  

--- EditCommand

data EditCommand db = Edit (EditM db ())
                 | AlertEdit String 
                 | ConfirmEdit String (EditCommand db)
                 | AuthenticateEdit ViewIdRef ViewIdRef
                 | LogoutEdit
                 deriving (Show, Typeable, Data)
                 
instance Eq (EditCommand db) where -- only changing the edit command does not
  c1 == c2 = True

class HasViewId v where
  getViewId :: v -> ViewId

instance HasViewId (WebView db) where
  getViewId (WebView viewId _ _ _ _) = viewId 
  
instance HasViewId w => HasViewId (Widget w) where
  getViewId (Widget _ _ w) = getViewId w 

instance HasViewId LabelView where
  getViewId = getLabelViewId

instance HasViewId (TextView db) where
  getViewId = getTextViewId

instance HasViewId RadioView where
  getViewId = getRadioViewId

instance HasViewId SelectView where
  getViewId = getSelectViewId

instance HasViewId (Button db) where
  getViewId = getButtonViewId

instance HasViewId (EditAction db) where
  getViewId = getActionViewId

instance HasViewId JSVar where
  getViewId = getJSVarViewId

--instance Show (a->a) where
--  show f = "<function>"
 
-- is text part of Button or the presentation?
-- is the action part of the button?
-- error handling! why don't internal errors show up on stderr?
-- id handling in views is buggy now

-- classes
{-
class Presentable v where
  present :: v -> Html

class Storable v where
  store :: v -> (Data -> Data)

-}

class Initial v where
  initial :: v

class Presentable v where
  present :: v -> Html

{-
class Presentable v => Viewable v where
  load :: Database -> v
  save :: v -> (Database -> Database)
-}

class Storeable db v where
  save :: v -> db -> db
  save _ = id

-- needed for initial of WebView, which has () as its view
instance Storeable db () where
  save () = id

type User = Maybe (String, String)

type WebNodeMap db = Map.Map ViewId (WebNode db)

-- TODO: why are stub id and id inside WebView instead of in WebViewNode?
data WebNode db = WebViewNode (WebView db)
                | WidgetNode  ViewId Id Id (AnyWidget db) -- ViewId StubId Id 
                  deriving (Show, Typeable, Data)


instance Eq (WebNode db) where
  (WidgetNode _ _ _ w1) == (WidgetNode _ _ _ w2) = w1 == w2 -- note that w1 and w2 are not Widget w, but AnyWidget
-- WebViews are always equal for diff, so descend into them to do a real eq.
  (WebViewNode (WebView _ _ _ _ wv1)) == (WebViewNode (WebView _ _ _ _ wv2)) = 
    case cast wv1 of
      Nothing -> False
      Just wv1' -> wv1' == wv2
  _ == _ = False             

                          
type ViewMap db = Map.Map ViewId (WebView db)

-- no class viewable, because mkView has parameters
data WebView db = forall view . ( Data (StateT (WebViewState db) IO view) 
                                , Initial view, Presentable view, Storeable db view
                                , Show view, Eq view, Data view) => 
                                WebView !ViewId !Id !Id (ViewId -> view -> WebViewM db view) !view
                             
               deriving Typeable
-- (viewId -> view -> WebViewM view) is the load view function. the parameters are the id and the old view (or initial)
-- view is the actual view (which is 'updated')
-- viewid is to identify the view and it's extra state.
-- first id is stub id, only used when presenting the first instance of the view
-- id is there for manipulating the dhtml trees. id's will be assigned automatically
-- why was this one existential again?


-- gunfold seems impossible! (maybe we don't need it)
-- recipe from this instance is from Data.Generics documentation
instance (Data db, Typeable db) => Data (WebView db) where
  gfoldl k z (WebView vi si i f v) = z WebView `k` vi `k` si `k` i `k` f `k` v
     
  gunfold k z c = error "gunfold not defined for WebView"
     
  toConstr (WebView _ _ _ _ _) = con_WebView 
  dataTypeOf _ = ty_WebView

ty_WebView = mkDataType "Views.WebView" [con_WebView]
con_WebView = mkConstr ty_WebView "WebView" [] Prefix

instance (Data state, Typeable state, Typeable x) =>Data (StateT state IO x) where
  gfoldl k z (StateT x) = z StateT `k` x
  gunfold k z c = error "gunfold not defined for StateT"
     
  toConstr (StateT _) = con_StateT 
  dataTypeOf _ = ty_StateT
  
  
ty_StateT = mkDataType "Control.Monad.StateT" [con_StateT]
con_StateT = mkConstr ty_StateT "StateT" [] Prefix

instance Show (WebView db) where
  show (WebView (ViewId i) _ _ _ v) = "<" ++ show i ++ ":" ++ show v ++ ">"

{- this one is not possible
instance Initial WebView where
  initial = WebView (ViewId (-1)) initial
-}
instance Presentable () where
  present () = "<initial webview>"
  
--instance Presentable WebView where
--  present (WebView _ _ _ _ v) = present v

instance Eq (WebView db) where
  _ == _ = True

-- webviews are always equal, so equality on views is not based on its sub views
-- (changes in the stucture of subviews is taken into account though, eq: [v1,v2] /= [v1,v2,newv])

instance Initial () where
  initial = ()
  
instance Initial Bool where
  initial = False

instance Initial [a] where
  initial = []

instance (Initial a1, Initial a2) => Initial (a1,a2) where
  initial = (initial, initial)

instance (Initial a1, Initial a2, Initial a3) => Initial (a1,a2,a3) where
  initial = (initial, initial, initial)

instance (Initial a1, Initial a2, Initial a3, Initial a4) => Initial (a1,a2,a3,a4) where
  initial = (initial, initial, initial, initial)

instance (Initial a1, Initial a2, Initial a3, Initial a4, Initial a5) => Initial (a1,a2,a3,a4,a5) where
  initial = (initial, initial, initial, initial, initial)

instance (Initial a1, Initial a2, Initial a3, Initial a4, Initial a5, Initial a6) => Initial (a1,a2,a3,a4,a5,a6) where
  initial = (initial, initial, initial, initial, initial, initial)

instance (Initial a1, Initial a2, Initial a3, Initial a4, Initial a5, Initial a6, Initial a7) => Initial (a1,a2,a3,a4,a5,a6,a7) where
  initial = (initial, initial, initial, initial, initial, initial, initial)

instance Initial (Maybe a) where
  initial = Nothing

instance Initial a => Initial (Either a b) where
  initial = Left initial

instance Initial Int where
  initial = 0

instance Initial Float where
  initial = 0.0

instance Initial Double where
  initial = 0.0

instance Initial w => Initial (Widget w) where
  initial = Widget noId noId initial
  
instance Initial LabelView where
  initial = LabelView noViewId ""

instance Initial (TextView db) where
  initial = TextView noViewId TextField "" Nothing

instance Initial RadioView where
  initial = RadioView noViewId [] 0 False

instance Initial SelectView where
  initial = SelectView noViewId [] 0 False

instance Initial (Button db) where
  initial = Button noViewId "" False "" "" (Edit $ return ())

instance Initial JSVar where
  initial = JSVar noViewId "" ""

instance Initial (EditAction db) where
  initial = EditAction noViewId (const $ Edit $ return ())  

instance Data db => Initial (WebView db) where
  initial = WebView (ViewId []) noId noId (\_ _ -> return ()) ()


data WebViewState db = 
  WebViewState { getStateUser :: User, getStateDb :: db, getStateViewMap :: (ViewMap db) 
               , getStatePath :: [Int], getStateViewIdCounter :: Int 
               , getStateSessionId :: SessionId -- not sure we really need the session ID here, but it doesn't do any harm
               , getStateHashArgs :: HashArgs
               } deriving (Typeable, Data)

type WebViewM db a = StateT (WebViewState db) IO a


type SessionId = Int

type SessionState db = (SessionId, User, db, WebView db, Maybe (EditCommand db), HashArgs) 
                     --(sessionId, user, db, rootView,   pendingEdit,            hashArgs)
                     
type SessionStateRef db = IORef (SessionState db)

type EditM db = StateT (SessionState db) IO 
-- TODO: maybe call this one SessionM or something like that?
--       it seems like we could use it in most of the functions in Server as well.

instance Show (EditM db a) where
  show _ = "{EditM _}"
  
type HashArgs = [(String,String)]
  
type RootViews db = [ (String, WebViewM db (WebView db)) ]
-- for keeping track of the root webviews