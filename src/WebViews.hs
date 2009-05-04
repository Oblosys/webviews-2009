{-# OPTIONS -fglasgow-exts #-}
module WebViews where

import Data.List
import Text.Html hiding (image)
import qualified Text.Html as Html
import Data.Generics
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Debug.Trace
import System.IO.Unsafe -- until we have monadic load view
import System.Time
import Types
import Database
import Generics
import WebViewLib
import HtmlLib

mkRootView :: User -> Database -> Int -> ViewMap -> WebView
mkRootView user db sessionId viewMap = 
  --mkPigView 3 (PigId 1) 5 user db viewMap 
  --mkVisitView sessionId (VisitId 1) user db viewMap
  mkVisitsView sessionId user db viewMap


-- Visits ----------------------------------------------------------------------  

data VisitsView = 
  VisitsView Int Int User [(String,String)] 
                 (Widget Button) (Widget Button) (Widget Button) (Widget Button) 
                 [Widget Button]
                 WebView
                 (Maybe WebView)
    deriving (Eq, Show, Typeable, Data)
  
modifyViewedVisit fn (VisitsView v a b c d e f g h i j) = (VisitsView (fn v) a b c d e f g h i j)

mkVisitsView sessionId = mkWebView (ViewId 2000) $
  \user db viewMap vid ->
  let (VisitsView oldViewedVisit _ _ _ _ _ _ _ _ _ _) = getOldView vid viewMap
      viewedVisit = constrain 0 (length visits - 1) oldViewedVisit
      (visitIds, visits) = unzip $ Map.toList $ allVisits db
  in  VisitsView viewedVisit
                 sessionId
                 user
                 [ (zipCode visit, date visit) | visit <- visits ]
                 (button (ViewId 2001) "Previous" (viewedVisit > 0) $ 
                    mkViewEdit (ViewId 2000) $ modifyViewedVisit (\x -> x-1)) 
                 (button (ViewId 2002) "Next" (viewedVisit < (length visits - 1)) $ 
                    mkViewEdit (ViewId 2000) $  modifyViewedVisit (+1))
                 (button (ViewId 2003) "Add" True addNewVisit)
                 (button (ViewId 2004) "Remove" (not $ null visits) $
                    ConfirmEdit ("Are you sure you want to remove this visit?") $ 
                                   (DocEdit $ removeVisit (visitIds !! viewedVisit)))
                 [ button (ViewId $ 2005+p) (show p) (p/=viewedVisit) (selectVisit (ViewId 2000) p) 
                 | p <- [0..length visits - 1] ]
                
                 (if user == Nothing then (mkLoginView user db viewMap) 
                                    else (mkLogoutView user db viewMap))
                 
                 (if null visits
                    then Nothing
                    else Just $ mkVisitView (visitIds !! viewedVisit) user db viewMap
                   )
 where addNewVisit = DocEdit $ \db -> let ((Visit nvid _ _ _),db') = newVisit db 
                                      in  updateVisit nvid (\v -> v {date = today}) db' 
       selectVisit vi v = mkViewEdit vi $ modifyViewedVisit (const v)

       today = unsafePerformIO $ -- only until we have a monad in mkView!! (probably doesn't even work now)
         do { clockTime <-  getClockTime
            ; ct <- toCalendarTime clockTime
            ; return $ show (ctDay ct) ++ "-" ++show (fromEnum (ctMonth ct) + 1) ++ "-" ++show (ctYear ct)
            }

instance Presentable VisitsView where
  present (VisitsView viewedVisit sessionId user visits prev next add remove selectButtons loginoutView mv) =
    withBgColor (Rgb 235 235 235) $ withPad 15 0 15 0 $
         (case user of
           Nothing -> present loginoutView 
           Just (_,name) -> stringToHtml ("Hello "++name++".") +++ present loginoutView) +++
    p << ("List of all visits     (session# "++show sessionId++")") +++         
    p << ( hList [ withBgColor (Rgb 250 250 250) $ boxed $ withSize 400 100 $ 
          (simpleTable [] [] $ 
            [ stringToHtml "Nr. ", stringToHtml "Zip  ", stringToHtml "Date"] :
            [ [ present selectButton
              , (if i == viewedVisit then bold  else id) $ stringToHtml zipCode 
              , (if i == viewedVisit then bold  else id) $ stringToHtml date 
              ] 
            | (i,selectButton,(zipCode, date)) <- zip3 [0..] selectButtons visits])])  +++
    p << (present add +++ present remove) +++
    p << ((if null visits then "There are no visits. " else "Viewing visit nr. "++ show (viewedVisit+1) ++ ".") +++ 
          "    " +++ present prev +++ present next) +++ 
    boxed (case mv of
             Nothing -> stringToHtml "No visits."
             Just pv -> present pv) 

instance Storeable VisitsView where
  save _ = id
   
instance Initial VisitsView where                 
  initial = VisitsView 0 initial initial initial initial initial initial initial initial initial initial
  
  

-- Visit -----------------------------------------------------------------------  

data VisitView = 
  VisitView VisitId (Widget EString) (Widget EString) Int (Widget Button) 
           (Widget Button) (Widget Button) [PigId] [String] [WebView]
    deriving (Eq, Show, Typeable, Data)

modifyViewedPig f (VisitView vid zipCode date viewedPig b1 b2 b3 pigs pignames mSubview) =
  VisitView vid zipCode date (f viewedPig) b1 b2 b3 pigs pignames mSubview

mkVisitView i = mkWebView (ViewId 0) $
  \user db viewMap vid -> 
  let (VisitView _ _ _ oldViewedPig _ _ _ _ _ mpigv) = getOldView vid viewMap
      (Visit visd zipcode date pigIds) = unsafeLookup (allVisits db) i
      nrOfPigs = length pigIds
      viewedPig = constrain 0 (nrOfPigs - 1) $ oldViewedPig
      pignames = map (pigName . unsafeLookup (allPigs db)) pigIds
  in  VisitView i (estr (ViewId 1000) zipcode) (estr (ViewId 1001) date) viewedPig 
                (button (ViewId 1003) "Previous" (viewedPig > 0) (previous (ViewId 0))) 
                (button (ViewId 1005) "Next" (viewedPig < (nrOfPigs - 1)) (next (ViewId 0)))
                (button (ViewId 1007) "Add" True (addPig visd)) pigIds pignames
                [mkPigView i pigId viewedPig user db viewMap | (pigId,i) <- zip pigIds [0..]]
 where -- next and previous may cause out of bounds, but on reload, this is constrained
       previous vi = mkViewEdit vi $ modifyViewedPig (\x -> x - 1)
       next vi = mkViewEdit vi $ modifyViewedPig (+1)
       addPig i = DocEdit $ addNewPig i 

addNewPig vid db =
  let ((Pig newPigId _ _ _ _), db') = newPig vid db      
  in  (updateVisit vid $ \v -> v { pigs = pigs v ++ [newPigId] }) db'

instance Presentable VisitView where
  present (VisitView vid zipCode date viewedPig b1 b2 b3 pigs pignames subviews) =
        
    p << ("Visit at zip code "+++ present zipCode +++" on " +++ present date) +++
    p << ("Visited "++ show (length pigs) ++ " pig" ++ 
          pluralS (length pigs) ++ ": " ++ listCommaAnd pignames) +++
    p << ((if null pigs 
           then stringToHtml $ "Not viewing any pigs.   " 
           else "Viewing pig nr. " +++ show (viewedPig+1) +++ ".   ")
           +++ present b1 +++ present b2) +++
    withPad 15 0 0 0 
      (hList $ map present subviews ++ [present b3] )

instance Storeable VisitView where
  save (VisitView vid zipCode date _ _ _ _ pigs pignames _) db =
    updateVisit vid (\(Visit _ _ _ pigIds) ->
                      Visit vid (getStrVal zipCode) (getStrVal date) pigIds)
                    db

instance Initial VisitView where
  initial = VisitView initial initial initial initial initial initial initial initial initial initial
                       



-- Pig -------------------------------------------------------------------------  

data PigView = PigView PigId String (Widget Button) Int Int (Widget EString) [Widget RadioView] (Either Int String) 
               deriving (Eq, Show, Typeable, Data)

mkPigView pignr i viewedPig = mkWebView (ViewId $ 10+pignr) $ 
      \user db viewMap vid ->
  let (Pig pid vid name [s0,s1,s2] diagnosis) = unsafeLookup (allPigs db) i
  in  PigView pid (imageUrl s0)
              (button (ViewId $ pignr*10000 + 10000) "remove" True ( ConfirmEdit ("Are you sure you want to remove pig "++show (pignr+1)++"?") $ 
                                   removePigAlsoFromVisit pid vid)) 
              viewedPig pignr (estr (ViewId $ pignr*10000 + 10001) name) 
                [ radioView (ViewId $ pignr*10000 + 10002) ["Pink", "Grey"] s0 True
                , radioView (ViewId $ pignr*10000 + 10003) ["Yes", "No"] s1 True
                , radioView (ViewId $ pignr*10000 + 10004) ["Yes", "No"] s2 (s1 == 0)
                ] diagnosis
 where removePigAlsoFromVisit pid vid =
         DocEdit $ removePig pid . updateVisit vid (\v -> v { pigs = delete pid $ pigs v } )  
       
       imageUrl s0 = "pig"++pigColor s0++pigDirection++".png" 
       pigColor s0 = if s0 == 1 then "Grey" else ""
       pigDirection = if viewedPig < pignr then "Left" 
                      else if viewedPig > pignr then "Right" else ""

instance Presentable PigView where
  present (PigView pid _ b _ pignr name [] diagnosis) = stringToHtml "initial pig"
  present (PigView pid imageUrl b viewedPig pignr name [co, ab, as] diagnosis) =
    withBgColor (if viewedPig == pignr then Rgb 200 200 200 else Rgb 225 225 225) $
      boxed $
        (center $ image $ imageUrl) +++
        (center $ " nr. " +++ show (pignr+1)) +++
        p << (center $ (present b)) +++
        p << ("Name:" +++ present name) +++
        p << "Pig color: " +++
        present co +++
        p << "Has had antibiotics: " +++
        present ab +++
        p << "Antibiotics successful: " +++
        present as

instance Storeable PigView where
  save (PigView pid _ _ _ _ name symptoms diagnosis) =
    updatePig pid (\(Pig _ vid _ _ diagnosis) -> 
                    (Pig pid vid (getStrVal name) (map getSelection symptoms) diagnosis)) 

instance Initial PigView where
  initial = PigView initial "" initial initial initial initial initial initial





