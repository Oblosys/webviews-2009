{-# LANGUAGE ExistentialQuantification, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ImpredicativeTypes, OverlappingInstances #-}
module Types where

import ObloUtils
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
                                                                 ; return $ unsafeRead ("Types.instance Read ViewId: takePathElts "++show str) digits : path
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

-- TODO: Maybe type families are useful for widgets?
data AnyWidget db = LabelWidget !(LabelView db)
                  | TextWidget !(TextView db)
                  | RadioViewWidget !(RadioView db) 
                  | SelectViewWidget !(SelectView db) 
                  | ButtonWidget !(Button db)
                  | JSVarWidget !(JSVar db) -- TODO: not really a widget, but until we know what it is, or what we should call widget, it is here 
                  | EditActionWidget !(EditAction db) -- TODO: not really a widget, but until we know what it is, or what we should call widget, it is here
                    deriving (Eq, Show, Typeable, Data)

-- Label

-- does not have a html counterpart. It is just a div with a view id that contains a string element
data LabelView db = LabelView { getLabelViewId :: ViewId, getLabelText :: String, getLabelStyle :: String } deriving (Show, Typeable, Data)
-- the db is only so we can declare instances (w db) for all widget types

instance Eq (LabelView db) where
  LabelView _ text1 style1 == LabelView _ text2 style2 = text1 == text2 && style1 == style2

labelView :: ViewId -> String -> String -> Widget (LabelView db)  
labelView viewId txt style = Widget noId noId $ LabelView viewId txt style

-- Text

-- todo rename Str stuff in Text

data TextType = TextField | PasswordField | TextArea deriving (Eq, Show, Typeable, Data)

data TextView db = TextView { getTextViewId :: ViewId, getTextType :: TextType, getStrVal' :: String
                            , getTextStyle :: String, getTextChange :: Maybe (String -> EditCommand db), getTextSubmit :: Maybe (EditCommand db) } deriving ( Show, Typeable, Data)

instance Eq (TextView db) where
  TextView _ t1 str1 style1 _ _ == TextView _ t2 str2 style2 _ _ = t1 == t2 && str1 == str2 && style1 == style2
  -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.
  
getStrVal (Widget _ _ (TextView vi h v _ _ _)) = v

textField :: ViewId -> String -> String -> Maybe (String -> EditCommand db) -> Maybe (EditCommand db) -> Widget (TextView db)
textField viewId str style mChangeAction mSubmitAction = Widget noId noId $ TextView viewId TextField str style mChangeAction mSubmitAction

passwordField :: ViewId -> String -> String -> Maybe (String -> EditCommand db) -> Maybe (EditCommand db) -> Widget (TextView db)
passwordField viewId str style mChangeAction mSubmitAction = Widget noId noId $ TextView viewId PasswordField str style mChangeAction mSubmitAction

textArea :: ViewId -> String -> String -> Maybe (String -> EditCommand db) -> Widget (TextView db)
textArea viewId str style mChangeAction = Widget noId noId $ TextView viewId TextArea str style mChangeAction Nothing

strRef (Widget _ _ (TextView (ViewId i) h _ _ _ _)) = ViewIdRef i


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

data RadioView db = RadioView { getRadioViewId :: ViewId, getItems :: [String], getRadioSelection' :: Int 
                              , getRadioEnabled :: Bool, getRadioStyle :: String, getRadioChange :: Maybe (Int -> EditCommand db)
                              } deriving (Show, Typeable, Data)
   
instance Eq (RadioView db) where
  RadioView _ items1 int1 enabled1 style1 _ == RadioView _ items2 int2 enabled2 style2 _ =
    items1 == items2 && int1 == int2 && enabled1 == enabled2 && style1 == style2
    -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.

instance HasSelection (RadioView db) where
  getSelection (RadioView i is sel _ _ _) = sel
  setSelection s (RadioView vi its _ en st ch) = RadioView vi its s en st ch

radioView :: ViewId -> [String] -> Int -> Bool -> String -> Maybe (Int -> EditCommand db) -> Widget (RadioView db)
radioView viewId its i enabled style mChangeAction = Widget noId noId $ RadioView viewId its i enabled style mChangeAction

-- SelectView

data SelectView db = SelectView { getSelectViewId :: ViewId, getSelectItems :: [String], getSelectSelection' :: Int 
                                , getSelectEnabled :: Bool, getSelectStyle :: String, getSelectChange :: Maybe (Int -> EditCommand db)
                                } deriving (Show, Typeable, Data)

instance Eq (SelectView db) where
  SelectView _ items1 int1 enabled1 style1 _ == SelectView _ items2 int2 enabled2 style2 _ =
    items1 == items2 && int1 == int2 && enabled1 == enabled2 && style1 == style2
    -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.

instance HasSelection (SelectView db) where
  getSelection (SelectView i is sel _ _ _) = sel
  setSelection s (SelectView vi its _ en st ch) = SelectView vi its s en st ch

selectView :: ViewId -> [String] -> Int -> Bool -> String -> Maybe (Int -> EditCommand db) -> Widget (SelectView db)
selectView viewId its i enabled style mChangeAction = Widget noId noId $ SelectView viewId its i enabled style mChangeAction
  
-- Button

data Button db = Button { getButtonViewId :: ViewId, buttonText :: String
                        , getButtonEnabled :: Bool, getButtonStyle :: String, getOnClick :: String 
                        , getCommand' :: EditCommand db 
                        } deriving (Show, Typeable, Data)

instance Eq (Button db) where
  Button _ txt1 enabled1 style1 onclick1 _ == Button _ txt2 enabled2 style2 onclick2 _ =
    txt1 == txt2 && enabled1 == enabled2 && style1 == style2 && onclick1 == onclick2
    -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.

button :: ViewId -> String -> Bool -> String -> String -> EditCommand db -> Widget (Button db)
button viewId txt enabled style onclick cmd = Widget noId noId $ Button viewId txt enabled style onclick cmd

-- JSVar

data JSVar db = JSVar { getJSVarViewId :: ViewId, getJSVarName :: String, getJSVarValue_ :: String } deriving (Show, Typeable, Data)
-- the db is only so we can declare instances (w db) for all widget types

instance Eq (JSVar db) where
  JSVar _ n1 v1 == JSVar _ n2 v2 = n1 == n2 && v1 == v2

-- renamed, so we can use jsVar for declaring a javascript variable. This constructor will probably be removed anyway
jsVar_ :: ViewId -> String -> String -> Widget (JSVar db)
jsVar_ viewId name value = Widget noId noId $ JSVar viewId name value

getJSVarValue (Widget _ _ jsv) = getJSVarValue_ jsv



-- EditAction

data EditAction db = EditAction { getActionViewId :: ViewId
                                , getCommand :: [String] -> EditCommand db -- edit actions can get parameters when executed from javascript 
                                } deriving (Show, Typeable, Data)

instance Eq (EditAction db) where
  EditAction _ _ == EditAction _ _ = True
  -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.


editAction :: ViewId -> ([String] -> EditCommand db) -> Widget (EditAction db)
editAction viewId cmd = Widget noId noId $ EditAction viewId cmd




--- EditCommand

data EditCommand db = Edit (EditM db ())
                 | AlertEdit String 
                 | ConfirmEdit String (EditCommand db)
                 | AuthenticateEdit ViewIdRef ViewIdRef
                 | LogoutEdit
                 deriving (Show, Typeable, Data)
                 
instance Eq (EditCommand db) where -- only changing the edit command does not
  c1 == c2 = True
  -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.
  
class HasViewId v where
  getViewId :: v -> ViewId

instance HasViewId (WebView db) where
  getViewId (WebView viewId _ _ _ _) = viewId 
  
instance HasViewId w => HasViewId (Widget w) where
  getViewId (Widget _ _ w) = getViewId w 

instance HasViewId (LabelView db) where
  getViewId = getLabelViewId

instance HasViewId (TextView db) where
  getViewId = getTextViewId

instance HasViewId (RadioView db) where
  getViewId = getRadioViewId

instance HasViewId (SelectView db) where
  getViewId = getSelectViewId

instance HasViewId (Button db) where
  getViewId = getButtonViewId

instance HasViewId (EditAction db) where
  getViewId = getActionViewId

instance HasViewId (JSVar db) where
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
  save _ = id -- the derived instance for webview takes care of recursively saving all webview children

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
                                , Show view, Eq view, Data view, MapWebView db view) => 
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

ty_WebView = mkDataType "Types.WebView" [con_WebView] 
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
  
instance Initial (LabelView db) where
  initial = LabelView noViewId "" ""

instance Initial (TextView db) where
  initial = TextView noViewId TextField "" "" Nothing Nothing

instance Initial (RadioView db) where
  initial = RadioView noViewId [] 0 False "" Nothing

instance Initial (SelectView db) where
  initial = SelectView noViewId [] 0 False "" Nothing

instance Initial (Button db) where
  initial = Button noViewId "" False "" "" (Edit $ return ())

instance Initial (JSVar db) where
  initial = JSVar noViewId "" ""

instance Initial (EditAction db) where
  initial = EditAction noViewId (const $ Edit $ return ())  

instance Data db => Initial (WebView db) where
  initial = WebView (ViewId []) noId noId (\_ _ -> return ()) ()

-- Alternative Generics method

-- recursive map on v arg in WebView is maybe handled a bit dodgy still.
class MapWebView db v where
  mapWebView :: (forall w . ( s -> WebView db -> (WebView db,s) 
                            , Data (w db) => s -> Widget (w db) -> (Widget (w db),s) -- This Data context requires ImpredicativeTypes :-(
                            , WidgetUpdates db
                            , Bool -- specifies whether map will recurse in WebView children
                            )
                ) -> v -> s -> (v, s)
  mapWebView fns x = pure x

data WidgetUpdates db = WidgetUpdates { labelViewUpdate :: LabelView db -> LabelView db
                                      , textViewUpdate :: TextView db -> TextView db  
                                      , radioViewUpdate :: RadioView db -> RadioView db  
                                      , selectViewUpdate :: SelectView db -> SelectView db  
                                      , buttonUpdate :: Button db -> Button db  
                                      , jsVarUpdate :: JSVar db -> JSVar db  
                                      , editActionUpdate :: EditAction db -> EditAction db  
                                      }
                                      
noWidgetUpdates :: WidgetUpdates db
noWidgetUpdates = WidgetUpdates id id id id id id id
                                    
instance MapWebView db (WebView db) where
  mapWebView fns@(fwv,_,_,recursive) wv state =
   case fwv state wv of
     (WebView a b c d v, state') ->  let (v', state'') | recursive = mapWebView fns v state'
                                                       | otherwise   = (v, state')
                                     in  (WebView a b c d v', state'')

instance (Data (w db), MapWebView db (w db)) => MapWebView db (Widget (w db)) where
  mapWebView fns@(_,fwd,_,_) wd state = 
    case fwd state wd of -- case not necessary here, but consistent with WebView instance
      (Widget sid id w, state') -> let (w', state'') = mapWebView fns w state' -- NOTE: recurse flag is only
                                   in  (Widget sid id w', state'')             -- used for WebViews, not widgets

instance MapWebView db (LabelView db) where
  mapWebView (_,_,widgetUpdates,_) w = pure $ labelViewUpdate widgetUpdates w 

instance MapWebView db (TextView db) where
  mapWebView (_,_,widgetUpdates,_) w = pure $ textViewUpdate widgetUpdates w 

instance MapWebView db (RadioView db) where
  mapWebView (_,_,widgetUpdates,_) w = pure $ radioViewUpdate widgetUpdates w 

instance MapWebView db (SelectView db) where
  mapWebView (_,_,widgetUpdates,_) w = pure $ selectViewUpdate widgetUpdates w 

instance MapWebView db (Button db) where
  mapWebView (_,_,widgetUpdates,_) w = pure $ buttonUpdate widgetUpdates w 

instance MapWebView db (JSVar db) where
  mapWebView (_,_,widgetUpdates,_) w = pure $ jsVarUpdate widgetUpdates w 

instance MapWebView db (EditAction db) where
  mapWebView (_,_,widgetUpdates,_) w = pure $ editActionUpdate widgetUpdates w 

instance MapWebView db a => MapWebView db (Maybe a) where
  mapWebView fns Nothing = pure Nothing
  mapWebView fns (Just a) = Just <$> mapWebView fns a

instance MapWebView db a => MapWebView db [a] where
  mapWebView fns []     = pure []
  mapWebView fns (a:as) = (:) <$> mapWebView fns a <*> mapWebView fns as 

instance (MapWebView db a, MapWebView db b) => MapWebView db (Either a b) where
  mapWebView fns (Left a)  = Left <$> mapWebView fns a
  mapWebView fns (Right a) = Right <$> mapWebView fns a

instance (MapWebView db a, MapWebView db b) => MapWebView db (a,b) where
  mapWebView fns (a,b) = (,) <$> mapWebView fns a <*> mapWebView fns b

instance MapWebView db ()

instance MapWebView db Bool

instance MapWebView db String

instance MapWebView db Int

instance MapWebView db Double


-- We could make this an instance of Applicative, but there don't seem to be many advantages (yet).
type F s a = s -> (a,s)

pure :: f -> F s f 
pure f state = (f, state) 

(<*>) :: F s (a->b) -> F s a -> F s b
f <*> x = \state -> let (f', state') = f state
                        (x', state'') = x state'
                    in  (f' x', state'')

(<$>) :: (a->b) -> F s a -> F s b
f <$> x = pure f <*> x

-- WebViewState

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