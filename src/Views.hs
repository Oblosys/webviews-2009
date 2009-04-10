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

class Presentable v => Viewable v where
  load :: Database -> v
  save :: v -> (Database -> Database)

newtype ViewId = ViewId Int deriving (Typeable, Data)

data WebView = forall view . (Presentable view, Show view, Data view) => WebView ViewId view
       deriving Typeable

instance Data WebView where
  gfoldl k z (WebView i v) = z WebView `k` i `k` v
     
  --gunfold k z c = case constrIndex c of
  --                  1 -> k (k (z WebView))
     
  toConstr (WebView _ _) = con_WebView 
  dataTypeOf _ = ty_WebView

ty_WebView = mkDataType "Views.WebView" [con_WebView]
con_WebView = mkConstr ty_WebView "WebView" [] Prefix


{-
data T a b = C1 a b | C2 deriving (Typeable, Data)

GHC will generate an instance that is equivalent to

 instance (Data a, Data b) => Data (T a b) where
     gfoldl k z (C1 a b) = z C1 `k` a `k` b
     gfoldl k z C2       = z C2

     gunfold k z c = case constrIndex c of
                         1 -> k (k (z C1))
                         2 -> z C2

     toConstr (C1 _ _) = con_C1
     toConstr C2       = con_C2

     dataTypeOf _ = ty_T

 con_C1 = mkConstr ty_T "C1" [] Prefix
 con_C2 = mkConstr ty_T "C2" [] Prefix
 ty_T   = mkDataType "Module.T" [con_C1, con_C2]

-}

instance Show WebView where
  show (WebView (ViewId i) v) = "<" ++ show i ++ ":" ++ show v ++ ">"

instance Presentable WebView where
  present (WebView _ v) = present v




-- don't use Presentable, because we might present strings in different ways.


presentRadioBox :: [String] -> EInt -> [Html]
presentRadioBox items (EInt (Id id) i) = radioBox (show id) items i

presentTextField :: EString -> Html
presentTextField (EString (Id id) str) =
  textfield "" ! [identifier (show id), strAttr "VALUE" str
                 , strAttr "onChange" $ "textFieldChanged('"++show id++"')"]


mkRootView db = 
  WebView (ViewId 0) $ mkVisitView db (VisitId 1)


mkVisitView db i = 
  let (Visit vid zipcode date pigIds) = unsafeLookup (allVisits db) i
      pignames = map (pigName . unsafeLookup (allPigs db)) pigIds
  in  VisitView i (estr zipcode) (estr date) pigIds pignames $
                case pigIds of
                  [] -> Nothing
                  (pigId:_) -> Just $ WebView (ViewId 1) $ mkPigView db 33 pigId

data VisitView = VisitView VisitId EString EString [PigId] [String] (Maybe WebView) deriving (Show, Typeable, Data)

instance Presentable VisitView where
  present (VisitView vid zipCode date pigs pignames mSubview) =
        h2 << ("Visit at "+++ presentTextField zipCode +++" on " +++ presentTextField date)
    +++ ("Visited "++ show (length pigs) ++" pigs:")
    +++ show pignames
    +++ case mSubview of
          Nothing -> stringToHtml "no pig"
          Just pv -> present pv


data PigView = PigView PigId Int EString [Int] (Either Int String) deriving (Show, Typeable, Data)

mkPigView db pignr i =
  let (Pig pid name symptoms diagnosis) = unsafeLookup (allPigs db) i
  in  PigView pid pignr (estr name) symptoms diagnosis

instance Presentable PigView where
  present (PigView pid pignr name symptoms diagnosis) =
    boxed $
        p << ("Pig nr. " +++ show pignr)
    +++ p << ("Name:" +++ presentTextField name)
    +++ p << ("Symptoms: "+++presentSymptoms symptoms+++" diagnosis "++show diagnosis)

presentSymptoms [a,b,c] = 
       p << "Type varken: " 
   +++ radioBox "q1" ["Roze", "Grijs"] a
   +++ p << "Fase cyclus: "
   +++ radioBox "q2" ["1", "2", "3"] b
   +++ p << "Haren overeind: "
   +++ radioBox "q3" ["Ja", "Nee"] c
 
-- HTML utils

updateReplaceHtml :: String -> Html -> Html
updateReplaceHtml targetId newElement =
  thediv![strAttr "op" "replace", strAttr "targetId" targetId ] 
    << newElement

mkDiv str elt = thediv![identifier str] << elt

boxed html = thediv![thestyle "border:solid; border-width:1px; padding:4px;"] << html

--radioBox :: String -> [String] -> Int -> Html
radioBox id items selectedIx =
  [ radio id (show i) ! ( [strAttr "onChange" ("debugAdd('boing');queueCommand('Set("++show id++","++show i++");')") ]
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

