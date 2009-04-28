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
                  deriving (Show, Read)

data Command = Init | Refresh | Test 
             | SetC Int String 
             | ButtonC Int 
             | ConfirmDialogOk 
               deriving (Show, Read) 


newtype Id = Id { unId :: Int } deriving (Show, Eq, Ord, Data, Typeable)
noId = Id (-1)

newtype IdRef = IdRef Int deriving (Show, Eq, Ord, Data, Typeable)
-- refs are different kind, because they may be part of view tree, and SYB id assignment functions 
-- should not affect them

mkRef (Id i) = IdRef i

data Widget w = Widget { getWidgetStubId :: Id, getWidgetId :: Id, getWidgetWidget :: w }
              deriving (Show, Typeable, Data)
                       
instance Eq (Widget w) where
  w1 == w2 = True

data EString = EString { getStrId' :: Id, getStrVal' :: String } deriving (Show, Typeable, Data)

instance Eq EString where
  EString _ str1 == EString _ str2 = str1 == str2
  
getStrId (Widget _ _ (EString i v)) = i

getStrVal (Widget _ _ (EString i v)) = v

estr str = Widget noId noId $ EString noId str

strRef (Widget _ _ (EString (Id i) _)) = IdRef i

data EInt = EInt { getIntId' :: Id, getIntVal' :: Int } deriving (Show, Typeable, Data)

instance Eq EInt where
  EInt _ int1 == EInt _ int2 = int1 == int2

getIntId (Widget _ _ (EInt i v)) = i

getIntVal (Widget _ _ (EInt i v)) = v

eint i = Widget noId noId $ EInt noId i

data Button = Button { getButtonId' :: Id, getCommand' :: EditCommand } deriving (Show, Typeable, Data)

instance Eq Button where
  b1 == b2 = True -- buttons are always equal for now

button cmd = Widget noId noId $ Button noId cmd 

data EditCommand = DocEdit (Database -> Database)
                 | ViewEdit (ViewId) (WebView -> WebView)
                 | AlertEdit String 
                 | ConfirmEdit String EditCommand
                 | AuthenticateEdit IdRef IdRef
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

newtype ViewId = ViewId Int deriving (Show, Eq, Ord, Typeable, Data)

type ViewMap = Map.Map ViewId WebView

-- no class viewable, because mkView has parameters
data WebView = forall view . ( Initial view, Presentable view, Storeable view
                             , Show view, Eq view, Data view) => 
                             WebView ViewId Id Id (User -> Database -> ViewMap -> ViewId -> view) view
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
  
instance Presentable WebView where
  present (WebView _ _ _ _ v) = present v




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
  initial = Widget noId noId initial
  
instance Initial EString where
  initial = EString (Id $ -1) ""

instance Initial EInt where
  initial = EInt (Id $ -1) 0

instance Initial Button where
  initial = Button noId (DocEdit id)

instance Initial WebView where
  initial = WebView (ViewId (-1)) noId noId (\_ _ _ _ -> ()) ()



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