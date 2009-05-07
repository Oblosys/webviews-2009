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
import System.Time
import Types
import Database
import Generics
import WebViewLib
import HtmlLib
import Control.Monad.State

mkRootView :: User -> Database -> Int -> ViewMap -> IO WebView
mkRootView user db sessionId viewMap = 
 do { let wvm = (mkVisitsView sessionId user db viewMap)
    ; rv <- evalStateT wvm (WebViewState 0)
    ; return $ assignIds rv
    } -- TODO: id's here?
-- TODO Make a function for this (also used in mkinitialrootview)




-- Visits ----------------------------------------------------------------------  

data VisitsView = 
  VisitsView Int Int User [(String,String)] 
                 WebView [EditAction] (Widget Button) (Widget Button) (Widget Button) (Widget Button) 
                 (Maybe WebView) [WebView] (Maybe (Widget Button))
    deriving (Eq, Show, Typeable, Data)
  
modifyViewedVisit fn (VisitsView v a b c d e f g h i j k l) = 
  VisitsView (fn v) a b c d e f g h i j k l

mkVisitsView sessionId = mkWebView $
 \user db viewMap vid ->
  do { let (VisitsView oldViewedVisit _ _ _ _ _ _ _ _ _ _ _ _) = getOldView vid viewMap
           viewedVisit = constrain 0 (length visits - 1) oldViewedVisit
           (visitIds, visits) = unzip $ Map.toList $ allVisits db
     ; today <- liftIO getToday             
     ; prevB   <- mkButton "Previous" (viewedVisit > 0)                   $ prev vid
     ; nextB   <- mkButton "Next"     (viewedVisit < (length visits - 1)) $ next vid 
     ; addB    <- mkButton "Add"      True                                $ addNewVisit today
     ; removeB <- mkButton "Remove" (not $ null visits) $
                    ConfirmEdit ("Are you sure you want to remove this visit?") $ 
                      DocEdit $ removeVisit (visitIds !! viewedVisit)
                  
     ; selectionActions <- sequence [ mkEditAction $ selectVisit vid p 
                                    | p <- [0..length visits - 1 ]
                                    ]

     ; loginOutView <- if user == Nothing then mkLoginView user db viewMap 
                                          else mkLogoutView user db viewMap
     
     ; visitView <-  if null visits then return Nothing
                     else do { vw <- mkVisitView (visitIds !! viewedVisit) user db viewMap
                             ; return (Just vw)
                             }
     ; commentViews <- sequence [ mkCommentView cid user db viewMap
                                | cid <- Map.keys (allComments db) ]                    
                       
     ; mAddCommentButton <- case user of 
                              Nothing -> return Nothing 
                              Just (_,name) -> fmap Just $ mkButton "Add a comment" True $ 
                                                 addComment name today
     ;  return $ VisitsView viewedVisit sessionId user
                 [ (zipCode visit, date visit) | visit <- visits ]
                 loginOutView selectionActions 
                 prevB nextB addB removeB  visitView commentViews mAddCommentButton
     }
 where prev vid = mkViewEdit vid $ modifyViewedVisit decrease
       next vid = mkViewEdit vid $ modifyViewedVisit increase
       
       addNewVisit today = DocEdit $ \db -> let ((Visit nvid _ _ _),db') = newVisit db 
                                            in  updateVisit nvid (\v -> v {date = today}) db' 
       selectVisit vi v = mkViewEdit vi $ modifyViewedVisit (const v)

       getToday =
         do { clockTime <-  getClockTime
            ; ct <- toCalendarTime clockTime
            ; return $ show (ctDay ct) ++ "-" ++show (ctMonth ct) ++ "-" ++show (ctYear ct) ++
                       ", "++show (ctHour ct) ++ ":" ++ show (ctMin ct)
            }
         
       addComment name today = 
         DocEdit $ \db -> let ((Comment ncid _ _ _), db') = newComment db
                          in  updateComment ncid (\v -> v { commentAuthor = name
                                                          , commentDate = today}) db'

instance Presentable VisitsView where
  present (VisitsView viewedVisit sessionId user visits loginoutView selectionActions  
                      prev next add remove mv commentViews mAddCommentButton) =
    withBgColor (Rgb 235 235 235) $ withPad 5 0 5 0 $    
    with_ [thestyle "font-family: arial"] $
      mkTableEx [width "100%"] [] [valign "top"]
       [[ ([],
           (h2 << "Piglet 2.0")  +++
           ("List of all visits     (session# "++show sessionId++")") +++         
      p << (hList [ withBgColor (Rgb 250 250 250) $ roundedBoxed Nothing $ withSize 200 100 $ 
             (let rowAttrss = [] :
                              [ [withEditActionAttr selectionAction] ++
                                if i == viewedVisit then [ fgbgColorAttr (Rgb 255 255 255) (Rgb 0 0 255)
                                                           ] else [] 
                              | (i,selectionAction) <- zip [0..] selectionActions 
                              ]
                  rows = [ stringToHtml "Nr.    ", stringToHtml "Zip"+++spaces 3
                         , (stringToHtml "Date"+++spaces 10) ]  :
                         [ [stringToHtml $ show i, stringToHtml zipCode, stringToHtml date] 
                         | (i, (zipCode, date)) <- zip [1..] visits
                         ]
              in  mkTable [strAttr "width" "100%", strAttr "cellPadding" "2", thestyle "border-collapse: collapse"] 
                     rowAttrss [] rows
               
                 )])  +++
      p << (present add +++ present remove) 
      )
      ,([align "right"],
     hList[
      case user of
         Nothing -> present loginoutView 
         Just (_,name) -> stringToHtml ("Hello "++name++".") +++ br +++ br +++ present loginoutView
      ] )]
      ]
     +++
      p << ((if null visits then "There are no visits. " else "Viewing visit nr. "++ show (viewedVisit+1) ++ ".") +++ 
             "    " +++ present prev +++ present next) +++ 
      boxed (case mv of
               Nothing -> stringToHtml "No visits."
               Just pv -> present pv)
     +++
      h2 << "Comments" +++
      vList (map present commentViews)
     +++ (case mAddCommentButton of 
            Nothing -> stringToHtml "Log in to add a comment"
            Just b  -> present b)
      
instance Storeable VisitsView where
  save _ = id
   
instance Initial VisitsView where                 
  initial = VisitsView 0 initial initial initial initial initial initial initial initial initial initial initial initial
  



-- Visit -----------------------------------------------------------------------  

data VisitView = 
  VisitView VisitId (Widget EString) (Widget EString) Int (Widget Button) 
           (Widget Button) (Widget Button) [PigId] [String] [WebView]
    deriving (Eq, Show, Typeable, Data)

modifyViewedPig f (VisitView vid zipCode date viewedPig b1 b2 b3 pigs pignames mSubview) =
  VisitView vid zipCode date (f viewedPig) b1 b2 b3 pigs pignames mSubview

mkVisitView i = mkWebView $
  \user db viewMap vid -> 
    do { let (VisitView _ _ _ oldViewedPig _ _ _ _ _ mpigv) = getOldView vid viewMap
             (Visit visd zipcode date pigIds) = unsafeLookup (allVisits db) i
             nrOfPigs = length pigIds
             viewedPig = constrain 0 (nrOfPigs - 1) $ oldViewedPig
             pignames = map (pigName . unsafeLookup (allPigs db)) pigIds
       
       ; zipT  <- mkTextField zipcode 
       ; dateT <- mkTextField date
       ; prevB <- mkButton "Previous" (viewedPig > 0)              $ previous vid
       ; nextB <- mkButton "Next"     (viewedPig < (nrOfPigs - 1)) $ next vid
       ; addB  <- mkButton "Add"      True                         $ addPig visd
                 
       ; pigViews <- sequence [ mkPigView vid i pigId viewedPig user db viewMap 
                              | (pigId,i) <- zip pigIds [0..] 
                              ]
                                     
       ; return $ VisitView i zipT dateT viewedPig prevB  nextB addB pigIds pignames pigViews 
       }
 where -- next and previous may cause out of bounds, but on reload, this is constrained
       previous vi = mkViewEdit vi $ modifyViewedPig decrease
       next vi = mkViewEdit vi $ modifyViewedPig increase
       addPig i = DocEdit $ addNewPig i 
      
addNewPig vid db = let ((Pig newPigId _ _ _ _), db') = newPig vid db      
                   in  (updateVisit vid $ \v -> v { pigs = pigs v ++ [newPigId] }) db'

instance Presentable VisitView where
  present (VisitView vid zipCode date viewedPig b1 b2 b3 pigs pignames subviews) =
    p << ("Visit at zip code "+++ present zipCode +++" on " +++ present date) +++
    p << ("Visited "++ show (length pigs) ++ " pig" ++  pluralS (length pigs) ++ ": " ++ 
          listCommaAnd pignames) +++
    p << ((if null pigs 
           then stringToHtml $ "Not viewing any pigs.   " 
           else "Viewing pig nr. " +++ show (viewedPig+1) +++ ".   ")
           +++ present b1 +++ present b2) +++
    withPad 15 0 0 0 (hList $ map present subviews ++ [present b3] )

instance Storeable VisitView where
  save (VisitView vid zipCode date _ _ _ _ pigs pignames _) =
    updateVisit vid (\(Visit _ _ _ pigIds) ->
                      Visit vid (getStrVal zipCode) (getStrVal date) pigIds)

instance Initial VisitView where
  initial = VisitView (VisitId initial) initial initial initial initial initial initial initial initial initial
                       




-- Pig -------------------------------------------------------------------------  

data PigView = PigView PigId EditAction String (Widget Button) Int Int (Widget EString) [Widget RadioView] (Either Int String) 
               deriving (Eq, Show, Typeable, Data)

mkPigView parentViewId pignr i viewedPig = mkWebView $ 
  \user db viewMap vid ->
   do { let (Pig pid vid name [s0,s1,s2] diagnosis) = unsafeLookup (allPigs db) i
      ; selectAction <- mkEditAction $ mkViewEdit parentViewId $ modifyViewedPig (\_ -> pignr)
      ; removeB <- mkButton "remove" True $ 
                     ConfirmEdit ("Are you sure you want to remove pig "++show (pignr+1)++"?") $ 
                       removePigAlsoFromVisit pid vid              
      ; nameT <- mkTextField name                             
      ; rv1 <- mkRadioView ["Pink", "Grey"] s0 True
      ; rv2 <- mkRadioView ["Yes", "No"]    s1 True
      ; rv3 <- mkRadioView ["Yes", "No"]    s2 (s1 == 0)
      ; return $ PigView pid selectAction (imageUrl s0) removeB  viewedPig pignr nameT 
                         [rv1, rv2, rv3] diagnosis
      }
 where removePigAlsoFromVisit pid vid =
         DocEdit $ removePig pid . updateVisit vid (\v -> v { pigs = delete pid $ pigs v } )  
       
       imageUrl s0 = "pig"++pigColor s0++pigDirection++".png" 
       pigColor s0 = if s0 == 1 then "Grey" else ""
       pigDirection = if viewedPig < pignr then "Left" 
                      else if viewedPig > pignr then "Right" else ""

instance Presentable PigView where
  present (PigView pid _ _ b _ pignr name [] diagnosis) = stringToHtml "initial pig"
  present (PigView pid editAction imageUrl b viewedPig pignr name [co, ab, as] diagnosis) =
       roundedBoxed (Just $ if viewedPig == pignr then Rgb 200 200 200 else Rgb 225 225 225) $
        (center $ withEditAction editAction $ image imageUrl) +++
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
  save (PigView pid _ _ _ _ _ name symptoms diagnosis) =
    updatePig pid (\(Pig _ vid _ _ diagnosis) -> 
                    (Pig pid vid (getStrVal name) (map getSelection symptoms) diagnosis)) 

instance Initial PigView where
  initial = PigView (PigId initial) initial "" initial initial initial initial initial initial


data CommentView = CommentView CommentId Bool String String String
                               (Maybe EditAction) (Maybe (Widget EString))--(Either (Maybe (Widget Button)) (Widget EString, Widget Button))
                   deriving (Eq, Show, Typeable, Data)

instance Initial CommentView where
  initial = CommentView (CommentId initial) initial initial initial initial initial initial

modifyEdited fn (CommentView a edited b c d e f) = (CommentView a (fn edited) b c d e f)

mkCommentView commentId = mkWebView $ \user db viewMap vid ->
 do { let (CommentView _ edited _ _ _ _ _) = getOldView vid viewMap
          (Comment _ author date text) =  unsafeLookup (allComments db) commentId
    
    ; editAction <- if edited
                    then fmap Just $ mkEditAction $ AlertEdit "submit"
                    else case  user of
                           Just (login, _) -> if login == author 
                                              then fmap Just $ mkEditAction $ mkViewEdit vid $ modifyEdited (const True)
                                              else return Nothing
                           _               -> return Nothing
 
    ; mTextField <- if edited
                    then fmap Just $ mkTextField text
                    else return $ Nothing
{-    ; widgets <- if not edited then
                   case user of
                     Just (login, _) -> if login == author 
                                        then fmap (Left . Just) $ 
                                               mkButton "Edit" True $ 
                                                 mkViewEdit vid $ modifyEdited (const True)
                                        else return $ Left Nothing
                     _               -> return $ Left Nothing
                 else
                  do { textEntry <- mkTextField text 
                     ; submitB <- mkButton "Submit" True $ AlertEdit "Submit"
                     ; return $ Right (textEntry, submitB)
                     }
-}    
    ; return $ CommentView commentId edited author date text editAction mTextField
    }

instance Storeable CommentView where
  save _ = id

instance Presentable CommentView where
  present (CommentView _ edited author date text mEditAction mTextField) =
    thediv![thestyle "border:solid; border-width:1px; padding:0px; width:500px;"] $
     (withBgColor (Rgb 225 225 225) $ thespan![thestyle "margin:4px;"] $
        ("posted by " +++ stringToHtml author +++ " on " +++ stringToHtml date)
       `leftRight`
        (case mEditAction of
           Just ea -> if edited then withEditAction ea $ stringToHtml "submit"
                                else withEditAction ea $ stringToHtml "edit"
           Nothing -> stringToHtml "")
     ) +++ 
     (withBgColor (Color "white") $ 
       thespan![thestyle "margin:4px;"] $ 
         case mTextField of 
           Nothing -> stringToHtml text
           Just textField -> present textField
           )