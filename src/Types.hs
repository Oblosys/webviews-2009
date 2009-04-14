{-# OPTIONS -fglasgow-exts #-}
module Types where

import Data.Generics
import Database

import Text.Html
import Text.Show.Functions

newtype Id = Id Int deriving (Show, Eq, Ord, Data, Typeable)
noId = Id (-1)

data EString = EString { getStrId :: Id, getStrVal :: String } deriving (Show, Typeable, Data)

estr str = EString noId str

data EInt = EInt { getIntId :: Id, getIntVal :: Int } deriving (Show, Typeable, Data)

eint i = EInt noId i

data Button = Button { getButtonId :: Id, getCommand :: EditCommand } deriving (Show, Typeable, Data)

data EditCommand = DocEdit (Database -> Database)
                 | ViewEdit (ViewId) (WebView -> WebView) deriving (Show, Typeable, Data)

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

-- For now, Storeable on root view must save its subviews explicitly. 
-- Maybe SYB can handle this (gives an ambiguous type var now)
-- otherwise template haskell can do this

newtype ViewId = ViewId Int deriving (Show, Eq, Typeable, Data)

-- no class viewable, because mkView has parameters
data WebView = forall view . (Initial view, Presentable view, Storeable view, Show view, Data view) => 
                             WebView ViewId (Database -> view -> view) view deriving Typeable
-- (view->view) is the load view function. It is not in a class because we want to parameterize it
-- view is the actual view (which is 'updated')

-- why was this one existential again?


-- gunfold seems impossible! (maybe we don't need it)
-- recipe from this instance is from Data.Generics documentation
instance Data WebView where
  gfoldl k z (WebView i f v) = z WebView `k` i `k` f `k` v
     
  gunfold k z c = error "gunfold not defined for WebView"
     
  toConstr (WebView _ _ _) = con_WebView 
  dataTypeOf _ = ty_WebView

ty_WebView = mkDataType "Views.WebView" [con_WebView]
con_WebView = mkConstr ty_WebView "WebView" [] Prefix



instance Show WebView where
  show (WebView (ViewId i) _ v) = "<" ++ show i ++ ":" ++ show v ++ ">"

{- this one is not possible
instance Initial WebView where
  initial = WebView (ViewId (-1)) initial
-}

instance Presentable WebView where
  present (WebView _ _ v) = present v

instance Storeable WebView where
  save (WebView _ _ v) = save v

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

instance Initial EString where
  initial = estr ""

instance Initial EInt where
  initial = eint 0

instance Initial Button where
  initial = Button noId (DocEdit id)