module Views where

import Control.Monad
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map 
import Text.Html
import Data.Generics

import Types
import Database

-- classes
{-
class Presentable v where
  present :: v -> Html

class Storable v where
  store :: v -> (Data -> Data)

-}

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

newtype ViewId = ViewId Int deriving (Typeable, Data)

-- no class viewable, because mkView has parameters
data WebView = forall view . (Presentable view, Storeable view, Show view, Data view) => 
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

instance Presentable WebView where
  present (WebView _ v) = present v

instance Storeable WebView where
  save (WebView _ v) = save v


-- don't use Presentable, because we might present strings in different ways.


presentRadioBox :: [String] -> EInt -> [Html]
presentRadioBox items (EInt (Id id) i) = radioBox (show id) items i

presentTextField :: EString -> Html
presentTextField (EString (Id id) str) =
  textfield "" ! [identifier (show id), strAttr "VALUE" str
                 , strAttr "onChange" $ "textFieldChanged('"++show id++"')"]


mkRootView db = 
  WebView (ViewId 0) $ mkVisitView db (VisitId 1)



data VisitView = VisitView VisitId EString EString EString [PigId] [String] (Maybe WebView) deriving (Show, Typeable, Data)

mkVisitView db i = 
  let (Visit vid zipcode date pigIds) = unsafeLookup (allVisits db) i
      pignames = map (pigName . unsafeLookup (allPigs db)) pigIds
  in  VisitView i (estr zipcode) (estr date) (estr "0") pigIds pignames $
                case pigIds of
                  [] -> Nothing
                  (pigId:_) -> Just $ WebView (ViewId 1) $ mkPigView db 33 pigId

instance Presentable VisitView where
  present (VisitView vid zipCode date viewedPig pigs pignames mSubview) =
        h2 << ("Visit at "+++ presentTextField zipCode +++" on " +++ presentTextField date)
    +++ ("Visited "++ show (length pigs) ++" pigs:")
    +++ show pignames
    +++ ("Viewing pig nr. " +++ presentTextField viewedPig)
    +++ case mSubview of
          Nothing -> stringToHtml "no pig"
          Just pv -> present pv

instance Storeable VisitView where
  save (VisitView vid zipCode date _ pigs pignames mSubView) db =
    let (Visit _ _ _ pigIds) = unsafeLookup (allVisits db) vid
        db' = case mSubView of
                Just v  -> save v db 
                Nothing -> db
    in  db' { allVisits = Map.insert vid (Visit vid (getStrVal zipCode) (getStrVal date) pigIds)
                                     (allVisits db')
            }
data PigView = PigView PigId Int EString [EInt] (Either Int String) deriving (Show, Typeable, Data)

mkPigView db pignr i =
  let (Pig pid name symptoms diagnosis) = unsafeLookup (allPigs db) i
  in  PigView pid pignr (estr name) (map eint symptoms) diagnosis

instance Presentable PigView where
  present (PigView pid pignr name symptoms diagnosis) =
    boxed $
        p << ("Pig nr. " +++ show pignr)
    +++ p << ("Name:" +++ presentTextField name)
    +++ p << ("Symptoms: "+++presentSymptoms symptoms+++" diagnosis "++show diagnosis)

presentSymptoms [a,b,c] = 
       p << "Type varken: " 
   +++ presentRadioBox ["Roze", "Grijs"] a
   +++ p << "Fase cyclus: "
   +++ presentRadioBox ["1", "2", "3"] b
   +++ p << "Haren overeind: "
   +++ presentRadioBox ["Ja", "Nee"] c
 
instance Storeable PigView where
  save (PigView pid _ name symptoms diagnosis) db =
    let (Pig _ _ _ diagnosis) = unsafeLookup (allPigs db) pid
    in  db { allPigs = Map.insert pid (Pig pid (getStrVal name) (map getIntVal symptoms) diagnosis)
                                     (allPigs db)
           }


saveUpdates :: WebView -> Database -> Database
saveUpdates rootView db = save rootView db

{-
getAllWebViews view = listify (\(_::WebView) -> True)

saveUpdate (WebView i v) db = save v db

saveUpdates :: WebView -> Database -> Database
saveUpdates rootView db =
  let webViews = getAllWebViews rootView
  in  db
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

unsafeLookup map key = 
  Map.findWithDefault
    (error $ "element "++ show key ++ " not found in " ++ show map) key map
-- do we want extra params such as pig nrs in sub views?
-- error handling database access
-- Unclear: we need both pig ids and subview ids?
-- make clear why we need an explicit view, and the html rep is not enough.

