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

newtype ViewId = ViewId Int

data WebView = forall view . WebView ViewId (view -> Html) (view -> String) view
       deriving Typeable

instance Data WebView where
  

instance Show WebView where
  show (WebView (ViewId i) _ s v) = "<" ++ show i ++ ":" ++ s v ++ ">"

instance Presentable WebView where
  present (WebView _ pres _ v) = pres v




-- don't use Presentable, because we might present strings in different ways.


presentRadioBox :: [String] -> EInt -> [Html]
presentRadioBox items (EInt (Id id) i) = radioBox (show id) items i

presentTextField :: EString -> Html
presentTextField (EString (Id id) str) =
  textfield "" ! [identifier (show id), strAttr "VALUE" str
                 , strAttr "onChange" $ "textFieldChanged('"++show id++"')"]



mkWebView :: (Presentable v, Show v) => Int -> (Database -> v) -> Database -> WebView
mkWebView i mkV db = WebView (ViewId i) present show $ mkV db 

mkRootView db = mkWebView 0 (mkVisitView $ VisitId 1) db
-- WebView (ViewId 0) present show $ mkVisitView db (VisitId 1)


mkVisitView i db = 
  let (Visit vid zipcode date pigIds) = unsafeLookup (allVisits db) i
      pignames = map (pigName . unsafeLookup (allPigs db)) pigIds
  in  VisitView i (estr zipcode) (estr date) pigIds pignames $
                case pigIds of
                  [] -> Nothing
                  (pigId:_) -> Just $ WebView (ViewId 1) present show $ mkPigView 33 pigId db

data VisitView = VisitView VisitId EString EString [PigId] [String] (Maybe WebView) deriving Show

instance Presentable VisitView where
  present (VisitView vid zipCode date pigs pignames mSubview) =
        h2 << ("Visit at "+++ presentTextField zipCode +++" on " +++ presentTextField date)
    +++ ("Visited "++ show (length pigs) ++" pigs:")
    +++ show pignames
    +++ case mSubview of
          Nothing -> stringToHtml "no pig"
          Just pv -> present pv


data PigView = PigView PigId Int EString [Int] (Either Int String) deriving Show

mkPigView pignr i db =
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

