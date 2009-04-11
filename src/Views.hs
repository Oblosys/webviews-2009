{-# OPTIONS -fglasgow-exts #-}
module Views where

import Control.Monad
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map 
import Text.Html
import Data.Generics

import Types
import Database
import Generics

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
                             WebView ViewId view deriving Typeable

-- no gunfold yet! (maybe we don't need it)
-- recipe from this instance is from Data.Generics documentation
instance Data WebView where
  gfoldl k z (WebView i v) = z WebView `k` i `k` v
     
  --gunfold k z c = case constrIndex c of
  --                  1 -> k (k (z WebView))
     
  toConstr (WebView _ _) = con_WebView 
  dataTypeOf _ = ty_WebView

ty_WebView = mkDataType "Views.WebView" [con_WebView]
con_WebView = mkConstr ty_WebView "WebView" [] Prefix



instance Show WebView where
  show (WebView (ViewId i) v) = "<" ++ show i ++ ":" ++ show v ++ ">"

{- this one is not possible
instance Initial WebView where
  initial = WebView (ViewId (-1)) initial
-}

instance Presentable WebView where
  present (WebView _ v) = present v

instance Storeable WebView where
  save (WebView _ v) = save v

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
  initial = Button noId id

-- don't use Presentable, because we might present strings in different ways.


presentRadioBox :: [String] -> EInt -> [Html]
presentRadioBox items (EInt (Id id) i) = radioBox (show id) items i

presentTextField :: EString -> Html
presentTextField (EString (Id id) str) =
  textfield "" ! [identifier (show id), strAttr "VALUE" str
                 , strAttr "onChange" $ "textFieldChanged('"++show id++"')"]

-- seems like this one could be in Present
presentButton :: String -> Button -> Html
presentButton text (Button (Id id) _) =
   primHtml $ "<button onclick=\"queueCommand('Button("++show id++");')\">"++text++"</button>"

mkRootView db = 
  WebView (ViewId 0) $ mkVisitView db (VisitId 1)



data VisitView = VisitView VisitId EString EString EString Button Button [PigId] [String] (Maybe WebView) deriving (Show, Typeable, Data)

mkVisitView db i = 
  let (Visit vid zipcode date vp pigIds) = unsafeLookup (allVisits db) i
      pignames = map (pigName . unsafeLookup (allPigs db)) pigIds
  in  VisitView i (estr zipcode) (estr date) (estr $ show vp) 
                (Button noId (previous vid)) (Button noId (next vid)) pigIds pignames $
                (Just $ WebView (ViewId 1) $ mkPigView db 33 (pigIds !! vp))
 where next vid     = updateVisit vid (\v -> v {viewedPig = viewedPig v + 1})
       previous vid = updateVisit vid (\v -> v {viewedPig = viewedPig v - 1})

instance Initial VisitView where
  initial = VisitView (VisitId (-1)) initial initial initial initial initial initial initial initial

instance Presentable VisitView where
  present (VisitView vid zipCode date viewedPig b1 b2 pigs pignames mSubview) =
        h2 << ("Visit at "+++ presentTextField zipCode +++" on " +++ presentTextField date)
    +++ ("Visited "++ show (length pigs) ++" pigs:")
    +++ show pignames
    +++ ("Viewing pig nr. " +++ presentTextField viewedPig)
    -- "debugAdd('boing');queueCommand('Set("++id++","++show i++");')"
    +++ presentButton "previous" b1
    +++ presentButton "next" b2
    +++ case mSubview of
          Nothing -> stringToHtml "no pig"
          Just pv -> present pv

instance Storeable VisitView where
  save (VisitView vid zipCode date _ _ _ pigs pignames mSubView) db =
    let db' = case mSubView of
                Just v  -> save v db 
                Nothing -> db
    in  updateVisit vid (\(Visit _ _ _ vp pigIds) ->
                          Visit vid (getStrVal zipCode) (getStrVal date) vp pigIds)
                    db'
data PigView = PigView PigId Int EString [EInt] (Either Int String) deriving (Show, Typeable, Data)

instance Initial PigView where
  initial = PigView (PigId (-1)) initial initial initial initial

mkPigView db pignr i =
  let (Pig pid name symptoms diagnosis) = unsafeLookup (allPigs db) i
  in  PigView pid pignr (estr name) (map eint symptoms) diagnosis

instance Presentable PigView where
  present (PigView pid pignr name [tv, kl, ho] diagnosis) =
    boxed $
        p << ("Pig nr. " +++ show pignr)
    +++ p << ("Name:" +++ presentTextField name)
    +++ p << "Symptoms: "
    +++ p << "Type varken: " 
    +++ presentRadioBox ["Roze", "Grijs"] tv
    +++ p << "Fase cyclus: "
    +++ presentRadioBox ["1", "2", "3"] kl
    +++ p << "Haren overeind: "
    +++ presentRadioBox ["Ja", "Nee"] ho
    +++ p << ("diagnosis " ++ show diagnosis)
 
instance Storeable PigView where
  save (PigView pid _ name symptoms diagnosis) =
    updatePig pid (\(Pig _ _ _ diagnosis) -> 
                    (Pig pid (getStrVal name) (map getIntVal symptoms) diagnosis)) 



-- where do these belong:

saveAllViews :: WebView -> Database -> Database
saveAllViews rootView db = save rootView db
-- save is recursive now

getWebViewById i view = 
  case listify (\(WebView i' _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: no button with id "
    _   -> error $ "internal error: multiple buttons with id "

getWebViewByViewId i vw =
  bla (\(Id i') -> i==i') (\(WebView i _) -> Just (\_ -> Just i)) Nothing vw

getAllWebViews view = listify (\(_::WebView) -> True) view
--saveViewById = save v db
{-
saveUpdatess :: WebView -> Database -> Database
saveUpdatess rootView db =
  let (webView : _) = getAllWebViews rootView
  in  saveAllUpdates webView db
-}
-- HTML utils

updateReplaceHtml :: String -> Html -> Html
updateReplaceHtml targetId newElement =
  thediv![strAttr "op" "replace", strAttr "targetId" targetId ] 
    << newElement

mkDiv str elt = thediv![identifier str] << elt

boxed html = thediv![thestyle "border:solid; border-width:1px; padding:4px;"] << html

--radioBox :: String -> [String] -> Int -> Html
radioBox id items selectedIx =
  [ radio id (show i) ! ( [strAttr "onChange" ("debugAdd('boing');queueCommand('Set("++id++","++show i++");')") ]
                          ++ if i == selectedIx then [strAttr "checked" ""] else []) 
                          +++ item +++ br 
                        | (i, item) <- zip [0..] items ]


htmlPage title bdy = 
  thehtml $ concatHtml [ header $ thetitle $ toHtml title
                       , body bdy 
                       ]


-- Utils

lputStr :: MonadIO m => String -> m ()
lputStr = liftIO . putStr

lputStrLn :: MonadIO m => String -> m ()
lputStrLn = liftIO . putStrLn

