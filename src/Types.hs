{-# OPTIONS -fglasgow-exts #-}
module Types where

import Data.Generics
import Database

import Text.Html
import Text.Show.Functions
import Data.Map (Map)
import qualified Data.Map as Map 
import Control.Monad.State
import Control.Monad.Identity

data Commands = Commands [Command] 
              | SyntaxError String -- for debugging post from client, replace read by Str in FromData instance
                  deriving (Eq, Show, Read)

getCommands (Commands cs) = cs
getCommands _             = []

data Command = Init | Refresh | Test 
             | SetC Int String 
             | ButtonC Int
             | PerformEditActionC Int 
             | ConfirmDialogOk 
               deriving (Eq, Show, Read) 

-- view id's are for identifying views and widgets with regard to incrementality
-- they remain constant over the view's/widget's life
-- for now, we assign them at mkView
newtype ViewId = ViewId Int deriving (Show, Eq, Ord, Typeable, Data)

noViewId = ViewId (-1)

newtype Id = Id { unId :: Int } deriving (Show, Eq, Ord, Data, Typeable)

noId = Id (-1)


-- refs are different kind, because they may be part of view tree, and SYB id assignment functions 
-- should not affect them
newtype ViewIdRef = ViewIdRef Int deriving (Show, Eq, Ord, Data, Typeable)

newtype IdRef = IdRef Int deriving (Show, Eq, Ord, Data, Typeable)

mkViewRef (ViewId i) = ViewIdRef i

mkRef (Id i) = IdRef i

data Widget w = Widget { getWidgetViewId :: ViewId, getWidgetStubId :: Id, getWidgetId :: Id, getWidgetWidget :: w }
              deriving (Show, Typeable, Data)
                       
instance Eq (Widget w) where
  w1 == w2 = True

data EString = EString { getStrId' :: Id, getHidden :: Bool, getStrVal' :: String } deriving (Show, Typeable, Data)

instance Eq EString where
  EString _ h1 str1 == EString _ h2 str2 = h1 == h2 && str1 == str2
  
getStrId (Widget _ _ _ (EString i h v)) = i

getStrVal (Widget _ _ _ (EString i h v)) = v

estr viewId str = Widget viewId noId noId $ EString noId False str

epassword viewId str = Widget viewId noId noId $ EString noId True str

strRef (Widget _ _ _ (EString (Id i) h _)) = IdRef i

data RadioView = RadioView { getIntId' :: Id, getItems :: [String], getSelection' :: Int 
                           , getRadioEnabled :: Bool 
                           } deriving (Show, Typeable, Data)

setSelection' :: Int -> RadioView -> RadioView 
setSelection' s (RadioView i its _ en) = RadioView i its s en

setSelection s (Widget vi si i rv) = Widget vi si i $ setSelection' s rv

instance Eq RadioView where
  RadioView _ items1 int1 enabled1 == RadioView _ items2 int2 enabled2 = 
    items1 == items2 && int1 == int2 && enabled1 == enabled2

getIntId (Widget _ _ _ (RadioView i is v _)) = i

getSelection (Widget _ _ _ (RadioView i is v _)) = v

radioView viewId its i enabled = Widget viewId noId noId $ RadioView noId its i enabled

data Button = Button { getButtonId' :: Id, buttonText :: String, getButtonEnabled :: Bool
                     , getCommand' :: EditCommand 
                     } deriving (Show, Typeable, Data)

instance Eq Button where
  Button _ txt1 enabled1 _ == Button _ txt2 enabled2 _ = txt1 == txt2 && enabled1 == enabled2

button viewId txt enabled cmd = Widget viewId noId noId $ Button noId txt enabled cmd


data EditAction = EditAction { getActionId :: Id, getCommand :: EditCommand 
                             } deriving (Show, Typeable, Data)

instance Eq EditAction where
  EditAction _ _ == EditAction _ _ = True
  

data EditCommand = DocEdit (Database -> Database)
                 | ViewEdit (ViewId) (WebView -> WebView)
                 | AlertEdit String 
                 | ConfirmEdit String EditCommand
                 | AuthenticateEdit ViewIdRef ViewIdRef
                 | LogoutEdit
                 deriving (Show, Typeable, Data)
                 
instance Eq EditCommand where -- only changing the edit command does not
  c1 == c2 = True

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

class Storeable v where
  save :: v -> Database -> Database

instance Storeable () where
  save () = id

type User = Maybe (String, String)

type WebNodeMap = Map.Map ViewId WebNode

data WebNode = WebViewNode WebView
             | WidgetNode  ViewId Id Id AnyWidget
               deriving (Show, Typeable, Data)

instance Eq WebNode where
  (WidgetNode _ _ _ w1) == (WidgetNode _ _ _ w2) = w1 == w2
-- WebViews are always equal for diff, so descend into them to do a real eq.
  (WebViewNode (WebView _ _ _ _ wv1)) == (WebViewNode (WebView _ _ _ _ wv2)) = 
    case cast wv1 of
      Nothing -> False
      Just wv1' -> wv1' == wv2
               

data AnyWidget = RadioViewWidget RadioView 
               | EStringWidget EString 
               | ButtonWidget Button  
               | EditActionWidget EditAction
                 deriving (Eq, Show, Typeable, Data)
                          
type ViewMap = Map.Map ViewId WebView

-- no class viewable, because mkView has parameters
data WebView = forall view . ( Initial view, Presentable view, Storeable view
                             , Show view, Eq view, Data view) => 
                             WebView ViewId Id Id (User -> Database -> ViewMap -> Int -> ViewId -> (view, Int)) view
                             deriving Typeable
-- (view->view) is the load view function. It is not in a class because we want to parameterize it
-- view is the actual view (which is 'updated')
-- viewid is to identify the view and it's extra state.
-- first id is stub id, only used when presenting the first instance of the view
-- id is there for manipulating the dhtml trees. id's will be assigned automatically
-- why was this one existential again?


-- gunfold seems impossible! (maybe we don't need it)
-- recipe from this instance is from Data.Generics documentation
instance Data WebView where
  gfoldl k z (WebView vi si i f v) = z WebView `k` vi `k` si `k` i `k` f `k` v
     
  gunfold k z c = error "gunfold not defined for WebView"
     
  toConstr (WebView _ _ _ _ _) = con_WebView 
  dataTypeOf _ = ty_WebView

ty_WebView = mkDataType "Views.WebView" [con_WebView]
con_WebView = mkConstr ty_WebView "WebView" [] Prefix



instance Show WebView where
  show (WebView (ViewId i) _ _ _ v) = "<" ++ show i ++ ":" ++ show v ++ ">"

{- this one is not possible
instance Initial WebView where
  initial = WebView (ViewId (-1)) initial
-}
instance Presentable () where
  present () = stringToHtml "<initial webview>"
  
--instance Presentable WebView where
--  present (WebView _ _ _ _ v) = present v




instance Eq WebView where
  _ == _ = True

-- webviews are always equal, so equality on views is not based on its sub views
-- (changes in the stucture of subviews is taken into account though, eq: [v1,v2] /= [v1,v2,newv])

instance Initial () where
  initial = ()
  
instance Initial [a] where
  initial = []

instance Initial (Maybe a) where
  initial = Nothing

instance Initial a => Initial (Either a b) where
  initial = Left initial

instance Initial String where
  initial = ""

instance Initial Int where
  initial = 0

instance Initial w => Initial (Widget w) where
  initial = Widget noViewId noId noId initial
  
instance Initial EString where
  initial = EString (Id $ -1) False ""

instance Initial RadioView where
  initial = RadioView (Id $ -1) [] 0 False

instance Initial Button where
  initial = Button noId "<button>" False (DocEdit id)

instance Initial EditAction where
  initial = EditAction noId (DocEdit id)  

instance Initial WebView where
  initial = WebView (ViewId (-1)) noId noId (\_ _ _ _ _ -> ((), error "Internal error: initial WebView evalutated")) ()



mkButton' str en ac vidC = (button (ViewId vidC) str en ac, vidC + 1)

mkButton :: String -> Bool -> EditCommand -> WVMonad (Widget Button)
mkButton str en ac = WV $ \vidC -> (button (ViewId vidC) str en ac, vidC + 1)

-- editActions don't really need a ViewId, but they do need their internal ids updates, which
-- is the easiest by putting them in Widgets.
mkEditAction ac = WV $ \vidC -> (Widget (ViewId vidC) noId noId $ EditAction noId ac, vidC + 1)

mkRadioView is s en = WV $ \vidC -> (radioView (ViewId vidC) is s en, vidC +1)

mkTextField str = WV $ \vidC -> (estr (ViewId vidC) str, vidC + 1)
mkPasswordField str = WV $ \vidC -> (epassword (ViewId vidC) str, vidC + 1)

widgetGetViewRef (Widget (ViewId vid) _ _ _) = ViewIdRef vid
                  
runWV :: Int -> WVMonad x -> (x, Int) 
runWV i (WV f) = f i

newtype WVMonad x = WV (Int -> (x, Int))

instance Monad WVMonad where
  return x = WV $ \i -> (x, i)   
  
  (WV ma) >>= f = WV $ \i -> let (a,i')  = ma i 
                                 (WV b) = f a 
                             in  b i'
  
type MkViewState v = [v->v]

-- Identity will be IO
type ViewM v a = StateT (MkViewState v) Identity a


mkAction :: (v->v) -> ViewM v Int
mkAction f = 
 do { actions <- get
    ; put $ actions ++ [f]
    ; return $ length actions + 1
    }


testMonad = print $ "bla" {- runMonad [1,2,3] $
 do { l <- getList
    ; return l
    } -}


-- hide this constructor, so user needs to use mkView
data AView v = AView { editActions :: [v->v], makeView :: v -> v }

mkView :: (ViewM v (v->v)) -> (AView v)
mkView mv = runIdentity $ 
 do { (mkview, state) <- runStateT mv []
    ; return $ AView state mkview
    }



data TestView = TestView Int String

--mkTestView
mkTestView v = mkView $ 
 do { i <- mkAction $ \(TestView x s) -> TestView x s
    ; return $ \v -> TestView i "bla"
    } 