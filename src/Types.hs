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

import Happstack.State
-- this imports the Typeable1 instance for StateT
-- somehow we cannot import Happstack.State.Types (package is always hidden)
-- Another weird thing is that in scion the instance is imported already

data Commands = Commands [Command] 
              | SyntaxError String -- for debugging post from client, replace read by Str in FromData instance
                  deriving (Eq, Show, Read)

getCommands (Commands cs) = cs
getCommands _             = []

data Command = Init | Refresh | Test 
             | SetC ViewId String 
             | ButtonC ViewId
             | SubmitC ViewId
             | PerformEditActionC ViewId 
             | ConfirmDialogOk 
               deriving (Eq, Show, Read) 

-- view id's are for identifying views and widgets with regard to incrementality
-- they remain constant over the view's/widget's life
-- for now, we assign them at mkView
newtype ViewId = ViewId [Int] deriving (Show, Read, Eq, Ord, Typeable, Data)

noViewId = ViewId []

newtype Id = Id { unId :: Int } deriving (Show, Eq, Ord, Data, Typeable)

noId = Id (-1)


-- refs are different kind, because they may be part of view tree, and SYB id assignment functions 
-- should not affect them
newtype ViewIdRef = ViewIdRef [Int] deriving (Show, Eq, Ord, Data, Typeable)

newtype IdRef = IdRef Int deriving (Show, Eq, Ord, Data, Typeable)

mkViewRef (ViewId i) = ViewIdRef i

mkRef (Id i) = IdRef i

data Widget w = Widget { getWidgetViewId :: ViewId, getWidgetStubId :: Id, getWidgetId :: Id, getWidgetWidget :: w }
              deriving (Show, Typeable, Data)
                       
instance Eq (Widget w) where
  w1 == w2 = True

data TextType = TextField | PasswordField | TextArea deriving (Eq, Show, Typeable, Data)

data Text = Text { getStrViewId' :: ViewId, getTextType :: TextType, getStrVal' :: String 
                 , getSubmitAction :: Maybe EditCommand } deriving (Show, Typeable, Data)

instance Eq Text where
  Text _ t1 str1 _ == Text _ t2 str2 _ = t1 == t2 && str1 == str2
  
getStrViewId (Widget _ _ _ (Text vi h v _)) = vi

getStrVal (Widget _ _ _ (Text vi h v _)) = v

textField viewId str mSubmitAction = Widget viewId noId noId $ Text viewId TextField str mSubmitAction

passwordField viewId str mSubmitAction = Widget viewId noId noId $ Text viewId PasswordField str mSubmitAction

textArea viewId str = Widget viewId noId noId $ Text viewId TextArea str Nothing

strRef (Widget _ _ _ (Text (ViewId i) h _ _)) = ViewIdRef i

data RadioView = RadioView { getIntViewId' :: ViewId, getItems :: [String], getSelection' :: Int 
                           , getRadioEnabled :: Bool 
                           } deriving (Show, Typeable, Data)

setSelection' :: Int -> RadioView -> RadioView 
setSelection' s (RadioView vi its _ en) = RadioView vi its s en

setSelection s (Widget vi si i rv) = Widget vi si i $ setSelection' s rv

instance Eq RadioView where
  RadioView _ items1 int1 enabled1 == RadioView _ items2 int2 enabled2 = 
    items1 == items2 && int1 == int2 && enabled1 == enabled2

getIntViewId (Widget _ _ _ (RadioView vi is v _)) = vi

getSelection (Widget _ _ _ (RadioView i is v _)) = v

radioView viewId its i enabled = Widget viewId noId noId $ RadioView viewId its i enabled

data Button = Button { getButtonViewId' :: ViewId, buttonText :: String, getButtonEnabled :: Bool
                     , getCommand' :: EditCommand 
                     } deriving (Show, Typeable, Data)

instance Eq Button where
  Button _ txt1 enabled1 _ == Button _ txt2 enabled2 _ = txt1 == txt2 && enabled1 == enabled2

button viewId txt enabled cmd = Widget viewId noId noId $ Button viewId txt enabled cmd


data EditAction = EditAction { getActionViewId :: ViewId, getCommand :: EditCommand 
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
  _ == _ = False             

data AnyWidget = RadioViewWidget RadioView 
               | TextWidget Text 
               | ButtonWidget Button  
               | EditActionWidget EditAction
                 deriving (Eq, Show, Typeable, Data)
                          
type ViewMap = Map.Map ViewId WebView

-- no class viewable, because mkView has parameters
data WebView = forall view . ( Data (StateT WebViewState IO view) 
                             , Initial view, Presentable view, Storeable view
                             , Show view, Eq view, Data view) => 
                             WebView ViewId Id Id (ViewId -> view -> WebViewM view) view
                             
               deriving Typeable
-- (viewId -> view -> WebViewM view) is the load view function. the parameters are the id and the old view (or initial)
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

instance (Typeable v) => Data (StateT WebViewState IO v) where
  gfoldl k z (StateT x) = z StateT `k` x
  gunfold k z c = error "gunfold not defined for StateT"
     
  toConstr (StateT _) = con_StateT 
  dataTypeOf _ = ty_StateT
  
  
ty_StateT = mkDataType "Control.Monad.StateT" [con_StateT]
con_StateT = mkConstr ty_StateT "StateT" [] Prefix



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
  
instance Initial Bool where
  initial = False

instance Initial [a] where
  initial = []

instance Initial (Maybe a) where
  initial = Nothing

instance Initial a => Initial (Either a b) where
  initial = Left initial

instance Initial Int where
  initial = 0

instance Initial w => Initial (Widget w) where
  initial = Widget noViewId noId noId initial
  
instance Initial Text where
  initial = Text noViewId TextField "" Nothing

instance Initial RadioView where
  initial = RadioView noViewId [] 0 False

instance Initial Button where
  initial = Button noViewId "<button>" False (DocEdit id)

instance Initial EditAction where
  initial = EditAction noViewId (DocEdit id)  

instance Initial WebView where
  initial = WebView (ViewId []) noId noId (\_ _ -> return ()) ()


data WebViewState = 
  WebViewState User Database ViewMap [Int] Int deriving (Typeable, Data)

type WebViewM a = StateT WebViewState IO a
