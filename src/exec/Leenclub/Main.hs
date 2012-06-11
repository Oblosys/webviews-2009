{-# OPTIONS -XDeriveDataTypeable -XPatternGuards -XMultiParamTypeClasses -XOverloadedStrings -XTemplateHaskell #-}
module Main where

import Data.List
import BlazeHtml
import Data.Generics
import Data.Char
import Data.Function
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Debug.Trace
import System.Time
import Types
import Generics
import WebViewPrim
import WebViewLib
import HtmlLib
import Control.Monad.State
import Server

import Database

import TemplateHaskell


main :: IO ()
main = server rootViews "LeenclubDB.txt" mkInitialDatabase lenders

rootViews :: [ (String, SessionId -> [String] -> WebViewM Database (WebView Database)) ]
rootViews = [ ("", mkLendersRootView), ("leners", mkLendersRootView), ("lener", mkLenderRootView), ("spullen", mkItemRootView) 
            , ("test", mkTestView)] 

{-
Doc

View id path does not necessarily correspond to the child order, but is based on construction order in the monad.
Plan:

id stubid in node? instead of in webview?

eq for WebNodes, do we need ViewId? Probably good to add, although mainly left and right operand are looked up based on a viewId,
so the viewId's will be equal.

document computeMove and get rid of Just _ <- matches
search items view
autocomplete
template view with menu bar


-}


-- Utils


unsafeLookupM dbf key = withDb $ \db -> unsafeLookup (dbf db) key

-- WebViews



data TestView = 
  TestView (Widget RadioView) (WebView Database) (WebView Database) 
    deriving (Eq, Show, Typeable, Data)

mkTestView :: SessionId -> [String] -> WebViewM Database (WebView Database)
mkTestView sessionId args = mkWebView $
  \vid oldTestView@(TestView radioOld _ _) ->
    do { radio <-  mkRadioView ["Naam", "Punten"] (getSelection radioOld) True
       ; let radioSel = getSelection radioOld
       ; (wv1, wv2) <- if True -- radioSel == 0
                       then do { wv1 <- mkHtmlView $ "een"
                               ; wv2 <- mkHtmlTemplateView "test.html" []
                               ; return (wv1,wv2)
                               }
                       else do { wv1 <- mkHtmlTemplateView "test.html" []
                               ; wv2 <- mkHtmlView $ "een"
                               ; return (wv1,wv2)
                               }
       ; liftIO $ putStrLn $ "radio value " ++ show radioSel
       ; let (wv1',wv2') = if radioSel == 0 then (wv1,wv2) else (wv2,wv1)
       ; let wv = TestView radio wv1' wv2'
       --; liftIO $ putStrLn $ "All top-level webnodes "++(show (everythingTopLevel webNodeQ wv :: [WebNode Database])) 
       
       ; return $ wv 
       }

instance Presentable TestView where
  present (TestView radio wv1 wv2) =
      vList [present radio, present wv1, present wv2]

instance Storeable Database TestView



data LendersRootView = 
  LendersRootView (Maybe String) (Widget (TextView Database)) (Widget (Button Database)) [WebView Database] 
                  (Widget RadioView) String
    deriving (Eq, Show, Typeable, Data)

mkLendersRootView :: SessionId -> [String] -> WebViewM Database (WebView Database)
mkLendersRootView sessionId args = mkWebView $
  \vid oldLenderView@(LendersRootView mSearchTerm tf _ _ radioOld _) ->
    do { let searchTerm = case args of 
                            []      -> ""
                            (arg:_) -> arg 
--                                       getStrVal tf
       ; searchField <- mkTextFieldAct searchTerm $ Edit $ return ()
       ; searchButton <- mkButtonWithClick "Search" True $ const ""

       ; let oldSortField = getSelection radioOld
       
       ; results <- if searchTerm == "" 
                    then return [] 
                    else withDb $ \db -> searchLenders searchTerm db
{-                            ; case lenders of
                                []   -> fmap singleton $ mkHtmlView $ "Geen resultaten voor zoekterm \""++searchTerm++"\""
                                lnrs -> mapM (mkLenderView Inline) lnrs
                            }
-}
-- todo: because of incrementality bug (or design flaw?) wv's need to be created in the order in which they appear in the tree 
-- todo: handling radio selection is awkward.
       ; liftIO $ putStrLn $ show oldLenderView
       ; sortFieldRadio <- mkRadioView ["Naam", "Punten"] (getSelection radioOld) True
       ; let sortFns = [ compare `on` lenderName, compare `on` lenderRating]
       ; let results' = sortBy (sortFns !! oldSortField) $ results
       
       ; resultWvs <- case results' of
                             []   -> fmap singleton $ mkHtmlView $ "Geen resultaten voor zoekterm \""++searchTerm++"\""
                             lnrs -> mapM (mkLenderView Inline) lnrs
       ; liftIO $ putStrLn $ "sortField: " ++ (show $ getSelection radioOld)
       ; return $ LendersRootView mSearchTerm searchField searchButton resultWvs sortFieldRadio $
                  jsScript $ --"/*"++show (ctSec ct)++"*/" ++
                    let navigateAction = jsNavigateTo $ "'/leners/'+"++jsGetWidgetValue searchField++";"
                    in  [ inertTextView searchField
                        , onClick searchButton navigateAction
                        , onSubmit searchField navigateAction
                        ]
       }
-- TODO: don't prevent submit when no textfield action is present!!!! (instead change script for this or something)
-- otherwise we cannot override return key for textfields without action

instance Presentable LendersRootView where
  present (LendersRootView searchTerm searchField searchButton lenderViews sortFieldRadio script) =
    mkLeenclubPage $
      hList [present searchField, present searchButton] +++
      vList (nbsp : present sortFieldRadio : map present lenderViews)
      +++ mkScript script





mkLenderRootView sessionId args = mkMaybeView "Onbekende lener" $
  case args of
    arg:_ -> do { mLender <- withDb $ \db -> Map.lookup (LenderId arg) (allLenders db)
                ; case mLender of
                    Nothing    -> return Nothing
                    Just lender -> fmap Just $ mkLenderView Full lender
                }
    _        -> return Nothing

mkItemRootView sessionId args = mkMaybeView "Onbekend item" $
  case args of
    arg:_  | Just i <- readMaybe arg -> 
      do { mItem <- withDb $ \db -> Map.lookup (ItemId i) (allItems db)
         ; case mItem of
                  Nothing    -> return Nothing
                  Just item -> fmap Just $ mkItemView Full item
         }
    _        -> return Nothing

instance Storeable Database LendersRootView

data Inline = Inline | Full deriving (Eq, Show, Typeable, Data)

isInline Inline = True
isInline Full   = False

data LenderView = 
  LenderView Inline Lender [WebView Database]
    deriving (Eq, Show, Typeable, Data)


{-
modifyViewedPig f (LenderView vid name) =
  LenderView vid zipCode date (f viewedPig) b1 b2 b3 pigs pignames mSubview
-}
--mkLenderView :: Int -> WebViewM Database (WebView Database)
mkLenderView inline lender = mkWebView $
  \vid oldLenderView@(LenderView _ _ _) -> 
    do { let itemIds = lenderItems lender
       ; items <- mapM (\itemId -> withDb $ \db -> unsafeLookup (allItems db) itemId) itemIds
       ; itemWebViews <- if isInline inline then return [] else mapM (mkItemView Inline) items
       --; ea <- mkEditAction $ Edit $ liftIO $ putStrLn "edit!!!\n\n"
       --; w <- mkRadioView ["Ja", "Nee"] 0 True
       --; w  <- mkButton "Test"      True         $ Edit $ return ()
       --; w <- mkTextField "test"
       --; t <- mkHtmlTemplateView "test.html" [("lender","Martijn")]
       ; return $ LenderView inline lender itemWebViews
       }
        
instance Presentable LenderView where
  present (LenderView Full lender itemWebViews) =
    mkLeenclubPage $
        vList [ h2 $ (toHtml $ "Lener " ++ lenderName lender)
              , span_ (presentRating 5 3) ! style "font-size: 20px"
              , hList [ boxedEx 1 $ (image ("leners/" ++ lenderLogin lender ++".jpg")) ! align "top"
                      , nbsp
                      , nbsp
                      , vList [ toHtml (lenderZipCode lender)
                              ]
                      ]
              , h2 << "Spullen"
              , vList $ map present itemWebViews
              ]
  present (LenderView Inline lender itemWebViews) =
    linkedLender lender $
      hList [ boxedEx 1 $ (image ("leners/" ++ lenderLogin lender ++".jpg") ! style "width: 30px") ! align "top"
            , vList [ toHtml (nbsp +++ nbsp +++ toHtml (lenderName lender))
                    ]
            ]
              
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
      case lender of
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
     {-
instance Presentable LenderView where
  present (LenderView vid zipCode date viewedPig b1 b2 b3 pigs pignames subviews) =
    withBgColor (Color white) $
    ("Lender at zip code "+++ present zipCode +++" on " +++ present date) +++ br +++
    p << ("Lenered "++ show (length pigs) ++ " pig" ++  pluralS (length pigs) ++ ": " ++ 
          listCommaAnd pignames) +++
    p << ((if null pigs 
           then stringToHtml $ "Not viewing any pigs.   " 
           else "Viewing pig nr. " +++ show (viewedPig+1) +++ ".   ")
           +++ present b1 +++ present b2) +++
    withPad 15 0 0 0 (hList' $ map present subviews) +++ present b3
-}

instance Storeable Database LenderView where


data ItemView = 
  ItemView Inline Item Lender
    deriving (Eq, Show, Typeable, Data)


mkItemView inline item = mkWebView $
  \vid oldItemView@(ItemView _ _ _) -> 
    do { owner <- unsafeLookupM allLenders (itemOwner item)
       ; return $ Just (item, owner)
       ; return $ ItemView inline item owner
       }
        
instance Presentable ItemView where
  present (ItemView Full item owner) =
    mkLeenclubPage $
        vList [ h2 $ (toHtml $ "Item " ++ itemName item)
              , hList [ boxedEx 1 $ (image ("items/" ++ (show $ itemIdNr item) ++".jpg") ! style "width: 200px") ! align "top"
                      , nbsp
                      , nbsp
                      , toHtml $ "Eigenaar: " ++ lenderName owner
                      , vList [ 
                              ]
                      ]
              ]
  present (ItemView Inline item owner) =
    linkedItem item $
      hList [ boxedEx 1 $ (image ("items/" ++ (show $ itemIdNr item) ++".jpg") ! style "width: 80px") ! align "top"
            , vList [ toHtml (nbsp +++ nbsp +++ toHtml (itemName item))
                    ]
            ]

instance Storeable Database ItemView

{-
mkItemView sessionId args = mkWebView $
  \vid oldItemView@(ItemView _) -> 
    do { mItemOwner <-
           case args of
             arg:_ -> do { mItem <- withDb $ \db -> Map.lookup (ItemId $ read arg) (allItems db)
                         ; case mItem of
                             Nothing -> return Nothing
                             Just item -> do { owner <- unsafeLookupM allLenders (itemOwner item)
                                             ; return $ Just (item, owner)
                                             }
                         }
             _        -> return Nothing

       ; return $ ItemView mItemOwner
       }
      
instance Presentable ItemView where
  present (ItemView mItemOwner)=
    mkLeenclubPage $
      case mItemOwner of
        Nothing           -> "Onbekend item"
        Just (item,owner) -> (h2 << (toHtml $ "Item " ++ itemName item)) +++ 
                             (h2 << (toHtml ("Eigenaar" :: String)))  +++
                             linkedLenderName owner
                      
-}




linkedItemName item@Item{itemId = ItemId i} = linkedItem item $ toHtml (itemName item)

linkedItem item@Item{itemId = ItemId login} html = 
  a ! (href $ (toValue $ "/spullen/" ++ (show login))) << html

linkedLenderName lender@Lender{lenderId = LenderId login} = linkedLender lender $ toHtml login
  
linkedLender lender@Lender{lenderId = LenderId login} html = 
  a! (href $ (toValue $ "/lener/" ++ login)) << html

mkLeenclubPage html = 
    mkPage [thestyle "background-color: #e0e0e0; font-family: arial"] $ 
      withPad 5 0 5 0 html    


 {-
-- Visits ----------------------------------------------------------------------  

data VisitsView = 
  VisitsView Bool Int Int Lender [(String,String)] 
                 (WebView Database) [EditAction Database] (Widget (Button Database)) (Widget (Button Database)) (Widget (Button Database)) (Widget (Button Database)) 
                 (WebView Database) [CommentId] [WebView Database] (Maybe (Widget (Button Database)))
    deriving (Eq, Show, Typeable, Data)
  
instance Initial VisitsView where                 
  initial = VisitsView True 0 initial initial initial initial initial initial initial initial initial initial initial initial initial

modifyViewedVisit fn (VisitsView a v b c d e f g h i j k l m n) = 
  VisitsView a (fn v) b c d e f g h i j k l m n

mkVisitsView sessionId = mkWebView $
 \vid (VisitsView fresh oldViewedVisit _ _ _ _ _  _ _ _ _ _ oldCommentIds _ _) ->
  do { (visitIds, visits) <- withDb $ (\db -> unzip $ Map.toList $ allVisits db)
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
                           
     ; lender <- getLender
               
     ; loginOutView <- if lender == Nothing then mkLoginView 
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
                       
     ; mAddCommentButton <- case lender of 
                              Nothing -> return Nothing 
                              Just (login,_) -> fmap Just $ mkButton "Add a comment" True $ 
                                                 addComment login today
     ;  return $ VisitsView False viewedVisit sessionId lender
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
                                                          , commentDate = today}) db'

instance Presentable VisitsView where
  present (VisitsView _ viewedVisit sessionId lender visits loginoutView selectionActions  
                      prev next add remove tabbedVisits _ commentViews mAddCommentButton) =
    withBgColor (Rgb 235 235 235) $ withPad 5 0 5 0 $    
    with [thestyle "font-family: arial"] $
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
      case lender of
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
      
instance Storeable Database VisitsView where
     



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
-}

deriveInitial ''TestView

deriveInitial ''LendersRootView

deriveInitial ''Inline

deriveInitial ''LenderView

instance Initial LenderId where
  initial = LenderId "_uninitializedId_"

deriveInitial ''Lender

deriveInitial ''ItemView

deriveInitial ''Item

instance Initial ItemId where
  initial = ItemId (-1)
