{-# OPTIONS -XDeriveDataTypeable -XPatternGuards -XMultiParamTypeClasses #-}
module Main where

import Data.List
import Data.List.Split
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
import Network.Curl.Download
import Types
import Generics
import WebViewPrim
import WebViewLib
import HtmlLib
import Control.Monad.State
import Server

import Database

main :: IO ()
main = server rootViews "TracDB.txt" mkInitialDatabase Map.empty

rootViews :: [ (String, SessionId -> [String] -> WebViewM Database (WebView Database)) ]
rootViews = [ ("", \args -> mkTicketsView)] -- ignore args 
    
-- Tickets ----------------------------------------------------------------------  

data TicketsView = 
  TicketsView [String]
    deriving (Eq, Show, Typeable, Data)
  
instance Initial TicketsView where                 
  initial = TicketsView initial


mkTicketsView sessionId = mkWebView $
 \vid (TicketsView str) ->
  do { contents <- liftIO $ fmap (tail . lines . either id id) $
                   openURIString "http://sourceforge.net/apps/trac/ampersand/report/12?format=tab&USER=anonymous"
     ; return $ TicketsView contents
     } {- (TicketIds, visits) <- withDb $ (\db -> unzip $ Map.toList $ allVisits db)
     ; let viewedVisit = constrain 0 (length visits - 1) oldViewedVisit
     ; today <- liftIO getToday             
     ; prevB   <- mkButton "Previous" (viewedVisit > 0)                   $ prev vid
     ; nextB   <- mkButton "Next"     (viewedVisit < (length visits - 1)) $ next vid 
     ; addB    <- mkButton "Add"      True                                $ addNewVisit today
     ; removeB <- mkButton "Remove" (not $ null visits) $
                    ConfirmEdit ("Are you sure you want to remove this visit?") $ 
                      Edit $ docEdit $ removeVisit (visitIds !! viewedVisit)
                  
     ; let selectionEdits = [ selectVisit vid v | v <- [0..length visits - 1 ] ]
                                    
     ; selectionActions <- mapM  (mkEditAction . Edit) selectionEdits
                           
     ; user <- getUser
               
     ; loginOutView <- if user == Nothing then mkLoginView 
                                          else mkLogoutView
     ; labels <- withDb $ \db -> map (zipCode . unsafeLookup (allVisits db)) visitIds
       
     ; visitViews <- uniqueIds $ [ (uniqueId, mkVisitView visitId)
                                 | (visitId@(VisitId uniqueId),i) <- zip visitIds [0..] 
                                 ]
     ; tabbedVisits <- mkTabbedView $ zip3 labels (map Just selectionEdits) visitViews
     --; let tabbedVisits = visitViews             
     --; let tabbedVisits = visitViews !! viewedVisit
     ; commentIds <- withDb $ \db -> Map.keys (allComments db)
                                    
     ; commentViews <- uniqueIds 
                          [ (uniqueId, mkCommentView cid (not fresh && cid `notElem` oldCommentIds)) 
                                                                    -- if this view is not fresh, and an
                          | cid@(CommentId uniqueId) <- commentIds  -- id was not in commentIds, it was
                          ]                                         -- added and will be in edit mode
                                               -- BUG: unfortunately, this also happens when it
                                               -- was introduced by a different session :-(
                       
     ; mAddCommentButton <- case user of 
                              Nothing -> return Nothing 
                              Just (login,_) -> fmap Just $ mkButton "Add a comment" True $ 
                                                 addComment login today
     ;  return $ VisitsView False viewedVisit sessionId user
                 [ (zipCode visit, date visit) | visit <- visits ]
                 loginOutView selectionActions 
                 prevB nextB addB removeB  tabbedVisits commentIds commentViews mAddCommentButton
     }
 where prev vid = Edit $ viewEdit vid $ modifyViewedVisit decrease
       next vid = Edit $ viewEdit vid $ modifyViewedVisit increase
       
       addNewVisit today = Edit $ docEdit $ \db -> let ((Visit nvid _ _ _),db') = newVisit db 
                                                   in  updateVisit nvid (\v -> v {date = today}) db' 
       selectVisit vi v = viewEdit vi $ modifyViewedVisit (const v)

       getToday =
         do { clockTime <-  getClockTime
            ; ct <- toCalendarTime clockTime
            ; return $ show (ctDay ct) ++ " " ++show (ctMonth ct) ++ " " ++show (ctYear ct) ++
                       ", "++show (ctHour ct) ++ ":" ++ reverse (take 2 (reverse $ "0" ++ show (ctMin ct)))
            }
         
       addComment login today = 
         Edit $ docEdit $ \db -> let ((Comment ncid _ _ _), db') = newComment db
                          in  updateComment ncid (\v -> v { commentAuthor = login
                                                          , commentDate = today}) db' -}

instance Presentable TicketsView where
  present (TicketsView contents)=
    withBgColor (Rgb 235 235 235) $ withPad 5 0 5 0 $    
    with [thestyle "font-family: arial"] $ mkTable [] [] [] [map stringToHtml (splitOn "\t" line) | line <- contents]
    
    {-
      mkTableEx [width "100%"] [] [valign "top"]
       [[ ([],
           (h2 << "Piglet 2.0")  +++
           ("List of all visits     (session# "++show sessionId++")") +++         
      p << (hList [ withBgColor (Rgb 250 250 250) $ roundedBoxed Nothing $ withSize 230 100 $ 
             (let rowAttrss = [] :
                              [ [withEditActionAttr selectionAction] ++
                                if i == viewedVisit then [ fgbgColorAttr (Rgb 255 255 255) (Rgb 0 0 255)
                                                           ] else [] 
                              | (i,selectionAction) <- zip [0..] selectionActions 
                              ]
                  rows = [ stringToHtml "Nr.    ", stringToHtml "Zip"+++nbspaces 3
                         , (stringToHtml "Date"+++nbspaces 10) ]  :
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
      ] +++
      p << ((if null visits then "There are no visits. " else "Viewing visit nr. "++ show (viewedVisit+1) ++ ".") +++ 
             "    " +++ present prev +++ present next) +++ 
      --vList (map present tabbedVisits)
      present tabbedVisits
{-
          boxed (case mv of
               [] -> stringToHtml "No visits."
               visitVs -> concatHtml $ map present visitVs) -} +++
      h2 << "Comments" +++
      vList (map present commentViews) +++ 
      nbsp +++ (case mAddCommentButton of 
                  Nothing -> stringToHtml "Please log in to add a comment"
                  Just b  -> present b)
      -}
instance Storeable Database TicketsView where
  save _ = id
     

{-

-- Visit -----------------------------------------------------------------------  

data VisitView = 
  VisitView VisitId (Widget (TextView Database)) (Widget (TextView Database)) Int (Widget (Button Database)) 
           (Widget (Button Database)) (Widget (Button Database)) [PigId] [String] [WebView Database]
    deriving (Eq, Show, Typeable, Data)

modifyViewedPig f (VisitView vid zipCode date viewedPig b1 b2 b3 pigs pignames mSubview) =
  VisitView vid zipCode date (f viewedPig) b1 b2 b3 pigs pignames mSubview

mkVisitView i = mkWebView $
  \vid (VisitView _ _ _ oldViewedPig _ _ _ _ _ mpigv) -> 
    do { (Visit visd zipcode date pigIds) <- withDb $ \db -> unsafeLookup (allVisits db) i
       ; let nrOfPigs = length pigIds
             viewedPig = constrain 0 (nrOfPigs - 1) $ oldViewedPig
       ; pignames <- withDb $ \db -> map (pigName . unsafeLookup (allPigs db)) pigIds
       
       ; zipT  <- mkTextField zipcode 
       ; dateT <- mkTextField date
       ; prevB <- mkButton "Previous" (viewedPig > 0)              $ previous vid
       ; nextB <- mkButton "Next"     (viewedPig < (nrOfPigs - 1)) $ next vid
       ; addB  <- mkButton "Add"      True                         $ addPig visd
                 
       ; pigViews <- uniqueIds $ [ (uniqueId, mkPigView vid i pigId viewedPig)
                                 | (pigId@(PigId uniqueId),i) <- zip pigIds [0..] 
                                 ]
                                     
       ; return $ VisitView i zipT dateT viewedPig prevB  nextB addB pigIds pignames pigViews 
       }
 where -- next and previous may cause out of bounds, but on reload, this is constrained
       previous vi = Edit $ viewEdit vi $ modifyViewedPig decrease
       next vi = Edit $ viewEdit vi $ modifyViewedPig increase
       addPig i = Edit $ docEdit $ addNewPig i 
      
addNewPig vid db = let ((Pig newPigId _ _ _ _), db') = newPig vid db      
                   in  (updateVisit vid $ \v -> v { pigs = pigs v ++ [newPigId] }) db'

instance Presentable VisitView where
  present (VisitView vid zipCode date viewedPig b1 b2 b3 pigs pignames subviews) =
    withBgColor (Color white) $
    ("Visit at zip code "+++ present zipCode +++" on " +++ present date) +++ br +++
    p << ("Visited "++ show (length pigs) ++ " pig" ++  pluralS (length pigs) ++ ": " ++ 
          listCommaAnd pignames) +++
    p << ((if null pigs 
           then stringToHtml $ "Not viewing any pigs.   " 
           else "Viewing pig nr. " +++ show (viewedPig+1) +++ ".   ")
           +++ present b1 +++ present b2) +++
    withPad 15 0 0 0 (hList' $ map present subviews) +++ present b3

instance Storeable Database VisitView where
  save (VisitView vid zipCode date _ _ _ _ pigs pignames _) =
    updateVisit vid (\(Visit _ _ _ pigIds) ->
                      Visit vid (getStrVal zipCode) (getStrVal date) pigIds)

instance Initial VisitView where
  initial = VisitView (VisitId initial) initial initial initial initial initial initial initial initial initial
                       

-- Pig -------------------------------------------------------------------------  

data PigView = PigView PigId (EditAction Database) String (Widget (Button Database)) Int Int (Widget (TextView Database)) (Widget (TextView Database)) [Widget RadioView] (Either Int String) 
               deriving (Eq, Show, Typeable, Data)

mkPigView parentViewId pignr pigId@(PigId pigInt) viewedPig = mkWebView $ 
  \vid (PigView _ _ _ _ _ _ oldViewStateT _ _ _) ->
   do { (Pig pid vid name [s0,s1,s2] diagnosis) <- withDb $ \db -> unsafeLookup (allPigs db) pigId
      ; selectAction <- mkEditAction $ Edit $ viewEdit parentViewId $ modifyViewedPig (\_ -> pignr)
      ; removeB <- mkButton "remove" True $ 
                     ConfirmEdit ("Are you sure you want to remove pig "++show (pignr+1)++"?") $ 
                       removePigAlsoFromVisit pid vid              
      ; nameT <- mkTextField name                             
      ; viewStateT <- mkTextField (getStrVal oldViewStateT)
      ; rv1 <- mkRadioView ["Pink", "Grey"] s0 True
      ; rv2 <- mkRadioView ["Yes", "No"]    s1 True
      ; rv3 <- mkRadioView ["Yes", "No"]    s2 (s1 == 0)
             
      ; return $ PigView pid selectAction (imageUrl s0) removeB viewedPig pignr 
                         viewStateT nameT [rv1, rv2, rv3] diagnosis
      }
 where removePigAlsoFromVisit pid vid =
         Edit $ docEdit $ removePig pid . updateVisit vid (\v -> v { pigs = delete pid $ pigs v } )  
       
       imageUrl s0 = "pig"++pigColor s0++pigDirection++".png" 
       pigColor s0 = if s0 == 1 then "Grey" else ""
       pigDirection = if viewedPig < pignr then "Left" 
                      else if viewedPig > pignr then "Right" else ""

instance Presentable PigView where
  present (PigView pid _ _ b _ _ pignr name [] diagnosis) = stringToHtml "initial pig"
  present (PigView pid editAction imageUrl b viewedPig pignr viewStateT name [co, ab, as] diagnosis) =
    withEditAction editAction $    
      roundedBoxed (Just $ if viewedPig == pignr then Rgb 200 200 200 else Rgb 225 225 225) $
        (center $ image imageUrl) +++
        (center $ " nr. " +++ show (pignr+1)) +++
        p << (center $ (present b)) +++
        p << ("Name:" +++ present name) +++
        p << "Pig color: " +++
        present co +++
        p << "Has had antibiotics: " +++
        present ab +++
        p << "Antibiotics successful: " +++
        present as +++ br +++
        "Note: " +++ present viewStateT
    
instance Storeable Database PigView where
  save (PigView pid _ _ _ _ _ _ name symptoms diagnosis) =
    updatePig pid (\(Pig _ vid _ _ diagnosis) -> 
                    (Pig pid vid (getStrVal name) (map getSelection symptoms) diagnosis)) 

instance Initial PigView where
  initial = PigView (PigId initial) initial "" initial initial initial initial initial initial initial


data CommentView = CommentView CommentId Bool String String String
                               (Maybe (WebView Database)) (Maybe (WebView Database)) (Maybe (Widget (TextView Database)))
                   deriving (Eq, Show, Typeable, Data)

instance Initial CommentView where
  initial = CommentView (CommentId initial) initial initial initial initial initial initial initial

modifyEdited fn (CommentView a edited b c d e f g) = (CommentView a (fn edited) b c d e f g)

mkCommentView commentId new = mkWebView $ \vid (CommentView _ edited' _ _ _ _ _ oldMTextfield) ->
 do { (Comment _ author date text) <- withDb $ \db -> unsafeLookup (allComments db) commentId
    
    ; let (_,name) = unsafeLookup users author
          
          edited = if new then True else edited' 
    
    ; submitButton <- mkLinkView "Submit" (Edit $ viewEdit vid $ modifyEdited (const False))
    ; editButton   <- mkLinkView "Edit"   (Edit $ viewEdit vid $ modifyEdited (const True))
    ; user <- getUser                
    ; let mEditAction = if edited
--                    then fmap Just $ mkButton "Submit" True $ mkViewEdit vid $ modifyEdited (const False)
                      then Just submitButton
                      else if userIsAuthorized author user 
                           then Just editButton
                           else Nothing
    ; removeAction <- mkLinkView "Remove" 
                        (ConfirmEdit ("Are you sure you want to remove this comment?") $ 
                          Edit $ docEdit $ removeComment commentId)
    ; let mRemoveAction = if userIsAuthorized author user 
                          then Just removeAction
                          else Nothing
      
    ; textArea <- mkTextArea text
    ; let mTextArea = if edited then Just textArea else Nothing
    
    ; return $ CommentView commentId edited name date text mEditAction mRemoveAction mTextArea
    }
 where userIsAuthorized authorLogin (Just (login, _)) = login == authorLogin || login == "martijn" 
       userIsAuthorized _           Nothing           = False

instance Storeable Database CommentView where
  save (CommentView cid edited _ date text _ _ mTextArea) =
    updateComment cid (\(Comment _ author _ _) -> 
                             let text' | edited, Just textArea <- mTextArea = getStrVal textArea       
                                       | otherwise                          = text
                             in  Comment cid author date text')

instance Presentable CommentView where
  present (CommentView _ edited author date text mEditAction mRemoveAction mTextArea) =
    thediv![thestyle "border:solid; border-width:1px; padding:0px; min-width: 550px;"] $
     (withBgColor (Rgb 225 225 225) $ --  thespan![thestyle "margin:4px;"] $
        (thespan![thestyle "margin:4px;"] $ "Posted by " +++ stringToHtml author +++ " on " +++ stringToHtml date)
        `hDistribute`
        (withColor (Color "blue") $
          case mEditAction of
           Just ea -> present ea
           Nothing -> stringToHtml ""
         +++ nbspaces 2 +++
         case mRemoveAction of
           Just ra -> present ra
           Nothing -> stringToHtml "") -- TODO: why is it not possible to add spaces behind this?
     ) +++ 
     (withBgColor (Color "white") $ 
        
         if edited then case mTextArea of 
                          Nothing -> thespan![thestyle "margin:4px;"] $ -- TODO: figure out why margin above creates too much space  
                                       multiLineStringToHtml text
                          Just textArea -> withHeight 100 $ present textArea
         else thespan![thestyle "margin:4px;"] $ -- TODO: figure out why margin above creates too much space  
                multiLineStringToHtml text) -}