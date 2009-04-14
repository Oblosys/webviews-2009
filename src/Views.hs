{-# OPTIONS -fglasgow-exts #-}
module Views where

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 
import Text.Html
import Data.Generics

import Types
import Database
import Generics

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


mkViewEdit i f = 
  ViewEdit i $ \(WebView i lv v) -> WebView i lv $ applyIfCorrectType f v


applyIfCorrectType :: (Typeable y, Typeable x) => (y -> y) -> x -> x
applyIfCorrectType f x = case cast f of 
                           Just fx -> fx x
                           Nothing -> x


-- the view matching on load can be done explicitly, following structure and checking ids, or
-- maybe automatically, based on id. Maybe extra state can be in a separate data structure even,
-- like in Proxima
mkRootView db = loadView db $
  WebView (ViewId 0) (mkVisitView (VisitId 1)) initial
  
loadView db (WebView i f v) = (WebView i f (f db v))

data VisitView = 
  VisitView VisitId EString EString EInt Button Button Button [PigId] [String] (Maybe WebView)
  deriving (Show, Typeable, Data)

-- todo: doc edits seem to reset viewed pig nr.
mkVisitView i db (VisitView _ _ _ oldViewedPig _ _ _ _ _ mpigv) = 
  let (Visit vid zipcode date pigIds) = unsafeLookup (allVisits db) i
      viewedPig = constrain 0 (length pigIds - 1) $ getIntVal oldViewedPig
      pignames = map (pigName . unsafeLookup (allPigs db)) pigIds
  in  VisitView i (estr zipcode) (estr date) (eint viewedPig) 
                (Button noId (previous (ViewId 0))) (Button noId (next (ViewId 0)))
                (Button noId (addPig vid)) pigIds pignames $
                let oldpigv = case mpigv of
                                  Nothing -> initial
                                  Just (WebView _ _ pigv) -> 
--                                    (initial `mkQ` (\pv -> pv::PigView)) pigv
                                    (initial `mkQ` (\pv@(PigView pid _ _ _ _ _) -> pv)) pigv
-- todo: check id's
                in (if null pigIds -- remove guard for weird hanging exception (after removing last pig)
                    then Nothing
                    else Just $ loadView db (WebView (ViewId 1)
                                            (mkPigView 33 (pigIds !! viewedPig)) 
                                            oldpigv)
                   )
 where -- next and previous may cause out of bounds, but on reload, this is constrained
       previous i = mkViewEdit i $
         \(VisitView vid zipCode date viewedPig b1 b2 b3 pigs pignames mSubview) ->
         VisitView vid zipCode date (eint $ getIntVal viewedPig-1) b1 b2 b3 pigs pignames mSubview

       next i = mkViewEdit i $
         \(VisitView vid zipCode date viewedPig b1 b2 b3 pigs pignames mSubview) ->
         VisitView vid zipCode date (eint $ getIntVal viewedPig+1) b1 b2 b3 pigs pignames mSubview

       addPig i = DocEdit $ addNewPig i 

addNewPig vid db =
  let ((Pig newPigId _ _ _ _), db') = newPig vid db      
  in  (updateVisit vid $ \v -> v { pigs = pigs v ++ [newPigId] }) db'

instance Initial VisitView where
  initial = VisitView (VisitId (-1)) initial initial initial initial initial initial initial initial initial

instance Presentable VisitView where
  present (VisitView vid zipCode date viewedPig b1 b2 b3 pigs pignames mSubview) =
        h2 << ("Visit at "+++ presentTextField zipCode +++" on " +++ presentTextField date)
    +++ ("Visited "++ show (length pigs) ++" pigs:")
    +++ show pignames
    +++ p << ((if null pigs then stringToHtml $ "Not viewing any pigs" 
               else "Viewing pig nr. " +++ show (getIntVal viewedPig))
    -- "debugAdd('boing');queueCommand('Set("++id++","++show i++");')"
              +++ presentButton "previous" b1 +++ presentButton "next" b2 +++ presentButton "add" b3)
    +++ p << (case mSubview of
               Nothing -> stringToHtml "no pigs"
               Just pv -> present pv)

instance Storeable VisitView where
  save (VisitView vid zipCode date _ _ _ _ pigs pignames mSubView) db =
    let db' = case mSubView of
                Just v  -> save v db 
                Nothing -> db
    in  updateVisit vid (\(Visit _ _ _ pigIds) ->
                          Visit vid (getStrVal zipCode) (getStrVal date) pigIds)
                    db'


data PigView = PigView PigId Button Int EString [EInt] (Either Int String) deriving (Show, Typeable, Data)

instance Initial PigView where
  initial = PigView (PigId (-1)) initial initial initial initial initial

mkPigView pignr i db oldPigView =
  let (Pig pid vid name symptoms diagnosis) = unsafeLookup (allPigs db) i
  in  PigView pid (Button noId (removePigAlsoFromVisit pid vid)) pignr (estr name) (map eint symptoms) diagnosis
 where -- need db support for removal and we need parent
       removePigAlsoFromVisit pid vid = 
         DocEdit $ removePig pid . updateVisit vid (\v -> v { pigs = delete pid $ pigs v } )  

instance Presentable PigView where
  present (PigView pid b pignr name [] diagnosis) = stringToHtml "initial pig"
  present (PigView pid b pignr name [tv, kl, ho] diagnosis) =
    boxed $
        p << ("Pig nr. " +++ show pignr +++ presentButton "remove" b)
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
  save (PigView pid _ _ name symptoms diagnosis) =
    updatePig pid (\(Pig _ vid _ _ diagnosis) -> 
                    (Pig pid vid (getStrVal name) (map getIntVal symptoms) diagnosis)) 


-- where do these belong:

saveAllViews :: WebView -> Database -> Database
saveAllViews rootView db = save rootView db
-- save is recursive now

getWebViewById i view = 
  case listify (\(WebView i' _ _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: no button with id "
    _   -> error $ "internal error: multiple buttons with id "

getWebViewByViewId i vw =
  bla (\(Id i') -> i==i') (\(WebView i _ _) -> Just (\_ -> Just i)) Nothing vw

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
  [ radio id (show i) ! ( [strAttr "onChange" ("queueCommand('Set("++id++","++show i++");')") ]
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

constrain mn mx x = (mn `max` x) `min` mx