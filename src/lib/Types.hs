{-# LANGUAGE ExistentialQuantification, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ImpredicativeTypes, OverlappingInstances, UndecidableInstances, LiberalTypeSynonyms #-}
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
             | DialogButtonPressed Int
               deriving (Eq, Show, Read) 


-- view id's are for identifying views and widgets with regard to incrementality
-- they remain constant over the view's/widget's life
-- for now, we assign them at mkView
newtype ViewId = ViewId [Int] deriving (Eq, Ord, Typeable)

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


newtype Id = Id { unId :: Int } deriving (Show, Eq, Ord)

noId = Id (-1)


-- refs are different type, because they may be part of view tree, and SYB id assignment functions 
-- should not affect them
newtype ViewIdRef = ViewIdRef [Int] deriving (Show, Eq, Ord)

newtype IdRef = IdRef Int deriving (Show, Eq, Ord)

mkViewRef (ViewId i) = ViewIdRef i

mkRef (Id i) = IdRef i

----- Widgets

data Widget w = Widget { getWidgetStubId :: Id, getWidgetId :: Id, getWidgetWidget :: w }
              deriving Show
                       
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
                    deriving (Eq, Show)

hasPresentation :: AnyWidget db -> Bool
hasPresentation (EditActionWidget _) = False -- EditActionWidgets have no presentation, so the incrementality algorithm won't create moves for them.
                                             -- They are also not instance of Present, so they cannot be presented. Instead, they stay completely in the Haskell world.
hasPresentation _                    = True

-- Label

-- does not have a html counterpart. It is just a div with a view id that contains a string element
data LabelView db = LabelView { getLabelViewId :: ViewId, getLabelText :: String, getLabelStyle :: String } deriving Show
-- the db is only so we can declare instances (w db) for all widget types

instance Eq (LabelView db) where
  LabelView _ text1 style1 == LabelView _ text2 style2 = text1 == text2 && style1 == style2

labelViewWidget :: ViewId -> String -> String -> Widget (LabelView db)  
labelViewWidget viewId txt style = Widget noId noId $ LabelView viewId txt style

-- Text

-- todo rename Str stuff in Text

data TextType = TextField | PasswordField | TextArea deriving (Eq, Show)

data TextView db = TextView { getTextViewId :: ViewId, getTextType :: TextType, getTextStrVal :: String
                            , getTextEnabled :: Bool, getTextStyle :: String, getTextChange :: Maybe (String -> EditM db ())
                            , getTextSubmit :: Maybe (EditM db ()) } deriving Show

instance Eq (TextView db) where
  TextView _ t1 str1 enabled1 style1 _ _ == TextView _ t2 str2 enabled2 style2 _ _ =
    t1 == t2 && enabled1 == enabled2 && str1 == str2 && style1 == style2
  -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.
  
getStrVal (Widget _ _ (TextView vi h v _ _ _ _)) = v

textFieldWidget :: ViewId -> String -> Bool -> String -> Maybe (String -> EditM db ()) -> Maybe (EditM db ()) -> Widget (TextView db)
textFieldWidget viewId str enabled style mChangeAction mSubmitAction = Widget noId noId $ TextView viewId TextField str enabled style mChangeAction mSubmitAction

passwordFieldWidget :: ViewId -> String -> Bool -> String -> Maybe (String -> EditM db ()) -> Maybe (EditM db ()) -> Widget (TextView db)
passwordFieldWidget viewId str enabled style mChangeAction mSubmitAction = Widget noId noId $ TextView viewId PasswordField str enabled style mChangeAction mSubmitAction

textAreaWidget :: ViewId -> String -> Bool -> String -> Maybe (String -> EditM db ()) -> Widget (TextView db)
textAreaWidget viewId str enabled style mChangeAction = Widget noId noId $ TextView viewId TextArea str enabled style mChangeAction Nothing

strRef (Widget _ _ (TextView (ViewId i) h _ _ _ _ _)) = ViewIdRef i


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

data RadioView db = RadioView { getRadioViewId :: ViewId, getItems :: [String], getRadioSelection :: Int 
                              , getRadioEnabled :: Bool, getRadioStyle :: String, getRadioChange :: Maybe (Int -> EditM db ())
                              } deriving Show
   
instance Eq (RadioView db) where
  RadioView _ items1 int1 enabled1 style1 _ == RadioView _ items2 int2 enabled2 style2 _ =
    items1 == items2 && int1 == int2 && enabled1 == enabled2 && style1 == style2
    -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.

instance HasSelection (RadioView db) where
  getSelection (RadioView i is sel _ _ _) = sel
  setSelection s (RadioView vi its _ en st ch) = RadioView vi its s en st ch

radioViewWidget :: ViewId -> [String] -> Int -> Bool -> String -> Maybe (Int -> EditM db ()) -> Widget (RadioView db)
radioViewWidget viewId its i enabled style mChangeAction = Widget noId noId $ RadioView viewId its i enabled style mChangeAction

-- SelectView

data SelectView db = SelectView { getSelectViewId :: ViewId, getSelectItems :: [String], getSelectSelection :: Int 
                                , getSelectEnabled :: Bool, getSelectStyle :: String, getSelectChange :: Maybe (Int -> EditM db ())
                                } deriving Show

instance Eq (SelectView db) where
  SelectView _ items1 int1 enabled1 style1 _ == SelectView _ items2 int2 enabled2 style2 _ =
    items1 == items2 && int1 == int2 && enabled1 == enabled2 && style1 == style2
    -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.

instance HasSelection (SelectView db) where
  getSelection (SelectView i is sel _ _ _) = sel
  setSelection s (SelectView vi its _ en st ch) = SelectView vi its s en st ch

selectViewWidget :: ViewId -> [String] -> Int -> Bool -> String -> Maybe (Int -> EditM db ()) -> Widget (SelectView db)
selectViewWidget viewId its i enabled style mChangeAction = Widget noId noId $ SelectView viewId its i enabled style mChangeAction
  
-- Button

data Button db = Button { getButtonViewId :: ViewId, buttonText :: String
                        , getButtonEnabled :: Bool, getButtonStyle :: String, getButtonOnClick :: String 
                        , getButtonCommand :: EditM db () 
                        } deriving Show

instance Eq (Button db) where
  Button _ txt1 enabled1 style1 onclick1 _ == Button _ txt2 enabled2 style2 onclick2 _ =
    txt1 == txt2 && enabled1 == enabled2 && style1 == style2 && onclick1 == onclick2
    -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.

buttonWidget :: ViewId -> String -> Bool -> String -> String -> EditM db () -> Widget (Button db)
buttonWidget viewId txt enabled style onclick cmd = Widget noId noId $ Button viewId txt enabled style onclick cmd

-- JSVar

data JSVar db = JSVar { getJSVarViewId :: ViewId, getJSVarName :: String, getJSVarValue_ :: String } deriving Show
-- the db is only so we can declare instances (w db) for all widget types

instance Eq (JSVar db) where
  JSVar _ n1 v1 == JSVar _ n2 v2 = n1 == n2 && v1 == v2

jsVarWidget :: ViewId -> String -> String -> Widget (JSVar db)
jsVarWidget viewId name value = Widget noId noId $ JSVar viewId name value

getJSVarValue (Widget _ _ jsv) = getJSVarValue_ jsv



-- EditAction

data EditAction db = EditAction { getEditActionViewId :: ViewId
                                , getEditActionCommand :: [String] -> EditM db () -- edit actions can get parameters when executed from javascript 
                                } deriving Show

instance Eq (EditAction db) where
  EditAction _ _ == EditAction _ _ = True
  -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.


editActionWidget :: ViewId -> ([String] -> EditM db ()) -> Widget (EditAction db)
editActionWidget viewId cmd = Widget noId noId $ EditAction viewId cmd



-- HasViewId
  
class HasViewId v where
  getViewId :: v -> ViewId

instance HasViewId (WebView db v) where
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
  getViewId = getEditActionViewId

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

type User = Maybe (String, String)

type WebNodeMap db = Map.Map ViewId (WebNode db)

-- TODO: why are stub id and id inside WebView instead of in WebViewNode?
data WebNode db = WebViewNode (UntypedWebView db)
                | WidgetNode  ViewId Id Id (AnyWidget db) -- ViewId StubId Id 
                  --deriving Show

instance Show (WebNode db)

instance Eq (WebNode db) where
  (WidgetNode _ _ _ w1) == (WidgetNode _ _ _ w2) = w1 == w2 -- note that w1 and w2 are not Widget w, but AnyWidget
-- WebViews are always equal for diff, so descend into them to do a real eq.
  (WebViewNode (UntypedWebView (WebView _ _ _ _ wv1))) == (WebViewNode (UntypedWebView (WebView _ _ _ _ wv2))) = 
    case cast wv1 of
      Nothing -> False
      Just wv1' -> wv1' == wv2
  _ == _ = False             

                          
type ViewMap db = Map.Map ViewId (UntypedWebView db)



                  
data WebView db view = WebView !ViewId !Id !Id (ViewId -> view -> WebViewM db view) !view
                             
-- (viewId -> view -> WebViewM view) is the load view function. the parameters are the id and the old view (or initial)
-- view is the actual view (which is 'updated')
-- viewid is to identify the view and it's extra state.
-- first id is stub id, only used when presenting the first instance of the view
-- id is there for manipulating the dhtml trees. id's will be assigned automatically
-- why was this one existential again?


instance Show v => Show (WebView db v) where
  show (WebView (ViewId i) _ _ _ v) = "<" ++ show i ++ ":" ++ show v ++ ">"

instance Eq (WebView db v) where
  _ == _ = True
-- webviews are always equal, so equality on views is not based on its sub views
-- (changes in the stucture of subviews is taken into account though, eq: [v1,v2] /= [v1,v2,newv])


-- Presentable and Storeable instances for WebView are declared in WebViewPrim

instance Initial v => Initial (WebView db v) where
  initial = WebView (ViewId []) noId noId (\_ _ -> return initial) initial

-- We don't use a class viewable, because mkView sometimes has extra parameters



-- Class IsWebView can be used as a shorthand for Show, Eq, ... MapWebView

class (Show v, Eq v, Presentable v, Storeable db v, Initial v, Typeable v, MapWebView db v, Typeable db) => IsWebView db v
      
instance (Show v, Eq v, Presentable v, Storeable db v, Initial v, Typeable v, MapWebView db v, Typeable db) => IsWebView db v


  
-- Wrapper for WebView to make the view type existential.

data UntypedWebView db =  forall view . IsWebView db view => UntypedWebView (WebView db view) 

mkUntypedWebView mkTypedWebView = fmap UntypedWebView mkTypedWebView

instance Show (UntypedWebView db) where
  show (UntypedWebView wv) = show wv

instance Eq (UntypedWebView db) where
  UntypedWebView v1 == UntypedWebView v2 = True

-- Presentable and Storeable instances for UntypedWebView are declared in WebViewPrim

instance Typeable db => Initial (UntypedWebView db) where
  initial = UntypedWebView $ (initial :: WebView db InitialView) 

instance MapWebView db (UntypedWebView db) where
  mapWebView (UntypedWebView v) = UntypedWebView <$> mapWebView v


-- For initial UntypedWebView we use a special value Initial which will never be presented or used, but needs all the IsWebView instances.

data InitialView = InitialView deriving (Show, Eq, Typeable) 

instance Presentable InitialView where
  present InitialView = "<InitialView>"

instance Storeable db InitialView where
  save InitialView = id

instance Initial InitialView where
  initial = InitialView

instance MapWebView db InitialView -- default instance is good


-- Initial class

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

instance Initial Html where
  initial = noHtml

instance Initial w => Initial (Widget w) where
  initial = Widget noId noId initial
  
instance Initial (LabelView db) where
  initial = LabelView noViewId "" ""

instance Initial (TextView db) where
  initial = TextView noViewId TextField "" False "" Nothing Nothing

instance Initial (RadioView db) where
  initial = RadioView noViewId [] 0 False "" Nothing

instance Initial (SelectView db) where
  initial = SelectView noViewId [] 0 False "" Nothing

instance Initial (Button db) where
  initial = Button noViewId "" False "" "" $ return ()

instance Initial (JSVar db) where
  initial = JSVar noViewId "" ""

instance Initial (EditAction db) where
  initial = EditAction noViewId (const $ return ())  

-- Alternative Generics method

-- recursive map on v arg in WebView is maybe handled a bit dodgy still.
class MapWebView db wv where
  mapWebView :: wv -> 
                (forall v w . ( IsWebView db v => WebView db v -> s -> (WebView db v,s) 
                              , MapWebView db (w db) => Widget (w db) -> s -> (Widget (w db),s) -- This MapWebView context requires ImpredicativeTypes :-(
                              , WidgetUpdates db s
                              , Bool -- specifies whether map will recurse in WebView children
                              )
                ) -> 
                s -> (wv, s)
  mapWebView x = pure x
{- By keeping the reader (ie. function tuple) and state arguments at the end, we can pass these around
   automatically in functions pure and <*>, which allows for very elegant instance declarations:
     mapWebView (Constr arg1 .. argn) = pure Constr <*> mapWebView arg1 <*> .. <*> mapWebView argn 

   For consistency, the same order is used in the functions in the function tuple and in WidgetUpdates.
-}

data WidgetUpdates db s = WidgetUpdates { labelViewUpdate  :: LabelView db  -> s -> (LabelView db, s)
                                        , textViewUpdate   :: TextView db   -> s -> (TextView db, s)
                                        , radioViewUpdate  :: RadioView db  -> s -> (RadioView db, s)  
                                        , selectViewUpdate :: SelectView db -> s -> (SelectView db, s) 
                                        , buttonUpdate     :: Button db     -> s -> (Button db, s)
                                        , jsVarUpdate      :: JSVar db      -> s -> (JSVar db, s)
                                        , editActionUpdate :: EditAction db -> s -> (EditAction db, s)  
                                        }
                                       
noWidgetUpdates :: WidgetUpdates db s
noWidgetUpdates = WidgetUpdates inert inert inert inert inert inert inert

inert x s = (x,s)
 
instance IsWebView db v => MapWebView db (WebView db v) where
  mapWebView wv fns@(fwv,_,_,recursive) state =
   case fwv wv state of
     (WebView a b c d v, state') ->  let (v', state'') | recursive = mapWebView v fns state'
                                                       | otherwise = (v, state')
                                     in  (WebView a b c d v', state'')

instance MapWebView db (w db) => MapWebView db (Widget (w db)) where
  mapWebView wd fns@(_,fwd,_,_) state = 
    case fwd wd state of -- case is not necessary here, but consistent with WebView instance
      (Widget sid id w, state') -> let (w', state'') = mapWebView w fns state' -- NOTE: recurse flag is only
                                   in  (Widget sid id w', state'')             -- used for WebViews, not widgets

instance MapWebView db (LabelView db) where
  mapWebView w (_,_,widgetUpdates,_) s = labelViewUpdate widgetUpdates w s 

instance MapWebView db (TextView db) where
  mapWebView w (_,_,widgetUpdates,_) s = textViewUpdate widgetUpdates w s 

instance MapWebView db (RadioView db) where
  mapWebView w (_,_,widgetUpdates,_) s = radioViewUpdate widgetUpdates w s 

instance MapWebView db (SelectView db) where
  mapWebView w (_,_,widgetUpdates,_) s = selectViewUpdate widgetUpdates w s 

instance MapWebView db (Button db) where
  mapWebView w (_,_,widgetUpdates,_) s = buttonUpdate widgetUpdates w s

instance MapWebView db (JSVar db) where
  mapWebView w (_,_,widgetUpdates,_) s = jsVarUpdate widgetUpdates w s

instance MapWebView db (EditAction db) where
  mapWebView w (_,_,widgetUpdates,_) s = editActionUpdate widgetUpdates w s

instance MapWebView db a => MapWebView db (Maybe a) where
  mapWebView Nothing = pure Nothing
  mapWebView (Just a) = Just <$> mapWebView a

instance MapWebView db a => MapWebView db [a] where
  mapWebView []     = pure []
  mapWebView (a:as) = (:) <$> mapWebView a <*> mapWebView as 

instance (MapWebView db a, MapWebView db b) => MapWebView db (Either a b) where
  mapWebView (Left a)  = Left <$> mapWebView a
  mapWebView (Right a) = Right <$> mapWebView a

instance (MapWebView db a1, MapWebView db a2) => MapWebView db (a1,a2) where
  mapWebView (a1,a2) = (,) <$> mapWebView a1 <*> mapWebView a2

instance (MapWebView db a1, MapWebView db a2, MapWebView db a3) => MapWebView db (a1,a2,a3) where
  mapWebView (a1,a2,a3) = (,,) <$> mapWebView a1 <*> mapWebView a2 <*> mapWebView a3

instance (MapWebView db a1, MapWebView db a2, MapWebView db a3, MapWebView db a4) => MapWebView db (a1,a2,a3,a4) where
  mapWebView (a1,a2,a3,a4) = (,,,) <$> mapWebView a1 <*> mapWebView a2 <*> mapWebView a3 <*> mapWebView a4

instance (MapWebView db a1, MapWebView db a2, MapWebView db a3, MapWebView db a4, MapWebView db a5) => MapWebView db (a1,a2,a3,a4,a5) where
  mapWebView (a1,a2,a3,a4,a5) = (,,,,) <$> mapWebView a1 <*> mapWebView a2 <*> mapWebView a3 <*> mapWebView a4 <*> mapWebView a5

instance (MapWebView db a1, MapWebView db a2, MapWebView db a3, MapWebView db a4, MapWebView db a5, MapWebView db a6) => MapWebView db (a1,a2,a3,a4,a5,a6) where
  mapWebView (a1,a2,a3,a4,a5,a6) = (,,,,,) <$> mapWebView a1 <*> mapWebView a2 <*> mapWebView a3 <*> mapWebView a4 <*> mapWebView a5 <*> mapWebView a6

instance (MapWebView db a1, MapWebView db a2, MapWebView db a3, MapWebView db a4, MapWebView db a5, MapWebView db a6, MapWebView db a7) => MapWebView db (a1,a2,a3,a4,a5,a6,a7) where
  mapWebView (a1,a2,a3,a4,a5,a6,a7) = (,,,,,,) <$> mapWebView a1 <*> mapWebView a2 <*> mapWebView a3 <*> mapWebView a4 <*> mapWebView a5 <*> mapWebView a6 <*> mapWebView a7

instance MapWebView db ()

instance MapWebView db Bool

instance MapWebView db String

instance MapWebView db Int

instance MapWebView db Double

instance MapWebView db Html



-- We could make this an instance of Applicative, but there don't seem to be many advantages (yet).
type F fns s a = fns -> s -> (a,s)

pure :: f -> F fns s f 
pure f = \fns state -> (f, state) 

(<*>) :: F fns s (a->b) -> F fns s a -> F fns s b
f <*> x = \fns state -> let (f', state') = f fns state
                            (x', state'') = x fns state'
                        in  (f' x', state'')

(<$>) :: (a->b) -> F fns s a -> F fns s b
f <$> x = pure f <*> x

-- WebViewState

data WebViewState db = 
  WebViewState { getWVStateUser :: User, getWVStateDb :: db, getWVStateViewMap :: (ViewMap db) 
               , getWVStatePath :: [Int], getWVStateViewIdCounter :: Int 
               , getWVStateSessionId :: SessionId -- not sure we really need the session ID here, but it doesn't do any harm
               , getWVStateHashArgs :: HashArgs
               } deriving Typeable

type WebViewM db a = StateT (WebViewState db) IO a

-- Typeable1 instance for StateT, which comes from happstack-state-6.1.4/src/Happstack/State/Types.hs 
-- (and has been updated to use mkTyCon3 instead of the deprecated mkTycon)
instance (Typeable st, Typeable1 m) => Typeable1 (StateT st m) where
    typeOf1 x = mkTyConApp (mkTyCon3 "mtl" "Control.Monad.State.Lazy" "StateT") [typeOf (undefined :: st), typeOf1 (m x)]
        where m :: StateT st m a -> m a
              m = undefined



type SessionId = Int

data SessionState db = SessionState { getSStateSessionId :: SessionId
                                    , getSStateUser :: User
                                    , getSStateDb :: db
                                    , getSStateRootView :: UntypedWebView db
                                    , getSStateDialogCommands :: Maybe [Maybe (EditM db ())]
                                    , getSStateHashArgs :: HashArgs
                                    } 
                     
type SessionStateRef db = IORef (SessionState db)


-- a subset of the session state that can be used in the EditM monad. 
data EditState db = EditState { getEStateAllUsers :: Map String (String, String)
                              , getEStateUser :: User
                              , getEStateDb :: db
                              , getEStateRootView :: UntypedWebView db
                              , getEStateScriptLines :: [String]
                              , getEStateDialog :: Maybe (Html ,[(String, Maybe (EditM db ()))])
                              }


type EditM db = StateT (EditState db) IO 

instance Show (EditM db a) where
  show _ = "{EditM _}"

instance Eq (EditM db ()) where
  c1 == c2 = True
  -- note that we don't need to look at the edit actions, since these live only in the Haskell world and have no effect on the html representation.

  


-- A class for state types that contain the database. Allows us to use the same functions in the WebViewM and EditM monads.
class HasDb state db where
  getStateDb :: state db -> db
  setStateDb :: db -> state db -> state db
  modifyStateDb :: (db -> db) -> state db -> state db
  
instance HasDb SessionState db where
  getStateDb sessionState = getSStateDb sessionState
  setStateDb db sessionState = sessionState{ getSStateDb = db }
  modifyStateDb f sessionState = sessionState{ getSStateDb = f $ getSStateDb sessionState }

instance HasDb EditState db where
  getStateDb editState = getEStateDb editState
  setStateDb db editState = editState{ getEStateDb = db }
  modifyStateDb f editState = editState{ getEStateDb = f $ getEStateDb editState }

instance HasDb WebViewState db where
  getStateDb wvState = getWVStateDb wvState
  setStateDb db wvState = wvState{ getWVStateDb = db }
  modifyStateDb f wvState = wvState{ getWVStateDb = f $ getWVStateDb wvState }
  

-- getDb :: WebViewM db db
-- getDb :: EditM db db
getDb :: (HasDb state db, Functor m, Monad m) => StateT (state db) m db
getDb = fmap getStateDb get

-- withDb :: (db -> a) -> WebViewM db a
-- withDb :: (db -> a) -> EditM db a
withDb :: (HasDb state db, Functor m, Monad m) => (db -> a) -> StateT (state db) m a
withDb f = fmap (f . getStateDb) $ get

-- modifyDb :: (db -> db) -> WebViewM db ()
-- modifyDb :: (db -> db) -> EditM db ()
modifyDb :: (HasDb state db, Functor m, Monad m) => (db -> db) -> StateT (state db) m ()
modifyDb f = modify (modifyStateDb f)

 
 
 
type HashArgs = [(String,String)]
  
type RootViews db = [ (String, WebViewM db (UntypedWebView db)) ]
-- for keeping track of the root webviews
 
-- Utility function that creates a RootViews tuple by applying UntypedWebView to webview builder function
-- It also takes the name parameter so we don't have to apply the function inside the tuple.
mkRootView :: IsWebView db v => String -> WebViewM db (WebView db v) -> (String, WebViewM db (UntypedWebView db))
mkRootView name mkTypedRootView = (name, fmap UntypedWebView mkTypedRootView)
