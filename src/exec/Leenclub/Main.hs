{-# OPTIONS -XDeriveDataTypeable -XPatternGuards -XMultiParamTypeClasses -XOverloadedStrings -XTemplateHaskell -XTupleSections -XFlexibleInstances -XScopedTypeVariables #-}
module Main where

import Data.List
import BlazeHtml
import Data.Generics
import Data.Char hiding (Space)
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

rootViews :: RootViews Database
rootViews = [ ("", mkLendersRootView), ("test", mkTestView), ("test2", mkTestView2 "msg")
            , ("leners", mkLendersRootView), ("lener", mkLenderRootView)
            , ("items", mkItemsRootView), ("item", mkItemRootView) 
            ] 

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

TODO: don't prevent submit when no textfield action is present!!!! (instead change script for this or something)
otherwise we cannot override return key for textfields without action


Ideas

composeView   $ \vid (a,b) -> do bla return (a,b)
instance Present (ComposeView ..)

Can't presentView simply supply a (PresentView )->Html?

mkWebViewInit (wv->wv) (wv->wv)       init arg is initial 

nice sort buttons with triangles



Home FAQ Profiel spullen geschiedenis Berichten aanmelden inloggen

-}


-- Utils


unsafeLookupM dbf key = withDb $ \db -> unsafeLookup (dbf db) key


--- Testing

data TestView = 
  TestView Int (Widget RadioView) (WebView Database) (WebView Database) (WebView Database) 
    deriving (Eq, Show, Typeable, Data)

mkTestView :: WebViewM Database (WebView Database)
mkTestView = mkWebView $
  \vid oldTestView@(TestView _ radioOld _ _ _) ->
    do { radio <-  mkRadioView ["Naam", "Punten", "Drie"] (getSelection radioOld) True
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
       ; presViewTest <- mkTestView2 $ "Radio value is " ++ show radioSel
       ; let (wv1',wv2') = if radioSel == 0 then (wv1,wv2) else (wv2,wv1)
       ; let wv = TestView radioSel radio wv1' wv2' presViewTest
       
       --; liftIO $ putStrLn $ "All top-level webnodes "++(show (everythingTopLevel webNodeQ wv :: [WebNode Database])) 
       
       ; return $ wv 
       }

instance Presentable TestView where
  present (TestView radioSel radio wv1 wv2 wv3) =
      vList [present radio, toHtml $ show radioSel, present wv1, present wv2, present wv3]

instance Storeable Database TestView


mkTestView2 msg = mkPresentView (\hs -> hList $ toHtml (msg :: String) : hs) $
    do { wv1 <- mkHtmlView $ "een"
       ; wv2 <- mkHtmlTemplateView "test.html" []
       ; return $ [wv1,wv2] 
       }


-- WebViews



data LendersRootView = LendersRootView (WebView Database)
    deriving (Eq, Show, Typeable, Data)

instance Storeable Database LendersRootView

mkLendersRootView :: WebViewM Database (WebView Database)
mkLendersRootView = mkWebView $
  \vid oldLenderView@(LendersRootView _) ->
    do { let namedSortFunctions = [ ("Naam",     compare `on` lenderName) 
                                  , ("Rating",    compare `on` lenderRating)
                                  ]
    
       ; searchView <- mkSearchView "Zoek in leners: " "q" $ \searchTerm ->
          do { results :: [Lender] <- if searchTerm == "" 
                    then return [] 
                    else do { resultLenders <- withDb $ \db -> searchLenders searchTerm db
                            ; return resultLenders
                            }
             ; mkSortView namedSortFunctions (mkLenderView Inline) results
             }
       ; return $ LendersRootView searchView
       }

instance Presentable LendersRootView where
  present (LendersRootView searchView) = mkLeenclubPage $ present searchView


mkLenderRootView = mkMaybeView "Onbekende lener" $
  do { args <- getHashArgs 
     ; case lookup "lener" args of
         Just lener -> do { mLender <- withDb $ \db -> Map.lookup (LenderId lener) (allLenders db)
                     ; case mLender of
                         Nothing    -> return Nothing
                         Just lender -> fmap Just $ mkLenderView Full lender
                     }
         Nothing    -> return Nothing
     }


data Inline = Inline | Full deriving (Eq, Show, Typeable, Data)

isInline Inline = True
isInline Full   = False

data LenderView = 
  LenderView Inline Lender [WebView Database]
    deriving (Eq, Show, Typeable, Data)

instance Storeable Database LenderView


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
              , span_ (presentRating 5 $ lenderRating lender) ! style "font-size: 20px"
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
            , nbsp
            , nbsp
            , vList [ toHtml (lenderName lender)
                    , span_ (presentRating 5 $ lenderRating lender) ! style "font-size: 20px"
                    ]
            ]


data ItemsRootView = 
  ItemsRootView (WebView Database) (WebView Database) (Widget (Button Database))
    deriving (Eq, Show, Typeable, Data)

instance Storeable Database ItemsRootView

mkItemsRootView ::WebViewM Database (WebView Database)
mkItemsRootView = mkWebView $
  \vid oldLenderView@(ItemsRootView _ _ _) ->
    do { let namedSortFunctions = [ ("Naam",     compare `on` itemName) 
                                  , ("Prijs",    compare `on` itemPrice)
                                  , ("Eigenaar", compare `on` itemDescr)
                                  ]
    
       ; searchView <- mkSearchView "Zoek in spullen:" "q" $ \searchTerm ->
          do { results :: [Item] <- if searchTerm == "" 
                    then return [] 
                    else do { resultItems <- withDb $ \db -> searchItems searchTerm db
                            ; return resultItems
                            }
             ; mkSortView namedSortFunctions (mkItemView Inline) results
             }
       ; dialogView <- mkDialogView $ mkHtmlView "dialog"
       ; dialogButton <- mkButton "Dialog" True $ Edit $ viewShowDialog dialogView 
       ; return $ ItemsRootView searchView dialogView dialogButton
       }
-- TODO: don't like the getViewId for dialogView, can we make a webview and return a result somehow?
--       or will typed webviews make this safe enough?

instance Presentable ItemsRootView where
  present (ItemsRootView searchView dialog dialogButton) =
        mkLeenclubPage $ present searchView >> present dialogButton >> present dialog



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
            , nbsp
            , nbsp
            , vList [ toHtml (itemName item)
                    , toHtml $ "Eigenaar: " ++ lenderName owner
                    , toHtml $ "Prijs:" ++ show (itemPrice item)
                    ]
            ]

instance Storeable Database ItemView




linkedItemName item@Item{itemId = ItemId i} = linkedItem item $ toHtml (itemName item)

linkedItem item@Item{itemId = ItemId login} html = 
  a ! (href $ (toValue $ "/#item&item=" ++ (show login))) << html

linkedLenderName lender@Lender{lenderId = LenderId login} = linkedLender lender $ toHtml login
  
linkedLender lender@Lender{lenderId = LenderId login} html = 
  a! (href $ (toValue $ "/#lener&lener=" ++ login)) << html

rootViewLink :: String -> Html -> Html 
rootViewLink rootViewName html = a ! (href $ (toValue $ "/#" ++ rootViewName)) << html

mkLeenclubPage html =  -- imdb: background-color: #E3E2DD; background-image: -moz-linear-gradient(50% 0%, #B3B3B0 0px, #E3E2DD 500px);  
    mkPage [thestyle $ gradientStyle (Just 500) "#444" {- "#B3B3B0" -} "#E3E2DD"  ++ " font-family: arial"] $ 
      div_ ! thestyle "border: 1px solid black; background-color: #f0f0f0; box-shadow: 0 0 8px rgba(0, 0, 0, 0.7);" $ 
        vList [ hStretchList (space :  concatMap (\(label,rootView) -> [E $ rootViewLink rootView $ label, space]) menuItems)
                  ! (thestyle $ "color: white; font-size: 16px;"++ gradientStyle Nothing "#707070" "#101010")
              , div_ ! thestyle "padding: 5px" $ html ] ! width "500px"
 where menuItems = [("Home",""), ("Leners", "leners"), ("Spullen", "items"), ("Login","login")]
 
gradientStyle :: Maybe Int -> String -> String -> String
gradientStyle mHeight topColor bottomColor =
    "background: -moz-linear-gradient("++topColor++" 0px, "++bottomColor++ maybe "" (\h -> " "++show h++"px") mHeight ++ "); "
  ++"background: -webkit-gradient(linear, left top, left "++maybe "bottom" show mHeight ++", from("++topColor++"), to("++bottomColor++"));"
  
  
  
  
  
  
  
  
data SortView db = 
  SortView (Widget SelectView) (Widget SelectView) [WebView db]  
    deriving (Eq, Show, Typeable, Data)

instance Data db => Initial (SortView db) where
  initial = SortView initial initial initial

instance Data db => Storeable db (SortView db)

-- [("sorteer oplopend", id), ("sorteer aflopend", reverse)]
-- [("Sorteer op naam", 
mkSortView :: [(String, a->a->Ordering)] -> (a-> WebViewM Database (WebView Database)) -> [a] -> WebViewM Database (WebView Database)
mkSortView namedSortFunctions mkResultWV results = mkWebView $
  \vid oldView@(SortView sortFieldSelectOld sortOrderSelectOld _) ->
    do { let sortField = getSelection sortFieldSelectOld
       ; let sortOrder = getSelection sortOrderSelectOld
       ; let (sortFieldNames, sortFunctions) = unzip namedSortFunctions
       ; sortFieldSelect <- mkSelectView sortFieldNames sortField True
       ; sortOrderSelect <- mkSelectView ["Oplopend", "Aflopend"] sortOrder True
       
       ; resultsWVs <- sequence [ fmap (r,) $ mkResultWV r | r <- results ]
       ; sortedResultViews <- case results of
                                [] -> fmap singleton $ mkHtmlView $ "Geen resultaten"
                                _  -> return $ map snd $ (if sortOrder == 0 then id else reverse) $ 
                                                         sortBy (sortFunctions !! sortField `on` fst) $ resultsWVs
    
       ; return $ SortView sortFieldSelect sortOrderSelect sortedResultViews
       }

instance Presentable (SortView db) where
  present (SortView sortFieldSelect sortOrderSelect webViews) = 
    (vList $ hStretchList [space, E $ "Sorteer" +++ nbsp, E $ present sortOrderSelect, E $ nbsp +++ "op" +++ nbsp, E $ present sortFieldSelect] 
              ! style "margin: 4 0 4 0"
            : intersperse hSep (map present webViews)
    ) ! style "width: 100%"        
   where hSep = div_ ! style "width: 100%; height:1px; background-color: black; margin: 5 0 5 0" $ noHtml
 
 

data SearchView db = 
  SearchView String (Widget (TextView db)) (Widget (Button db)) (WebView db) String 
    deriving (Eq, Show, Typeable, Data)

instance Data db => Initial (SearchView db) where
  initial = SearchView initial initial initial initial initial

instance Data db => Storeable db (SearchView db)

-- todo add mSearchTerm
mkSearchView label argName resultsf = mkWebView $
  \vid oldView@( SearchView _ _ _ _ _) ->
    do { args <- getHashArgs
       ; let searchTerm = case lookup argName args of 
                            Nothing    -> ""
                            Just term -> term 
       ; searchField <- mkTextFieldAct searchTerm $ Edit $ return ()
       ; searchButton <- mkButtonWithClick "Search" True $ const ""
       ; results <- resultsf searchTerm
       ; return $ SearchView label searchField searchButton results $
                  jsScript $
                    let navigateAction = "setHashArg('"++argName++"', "++jsGetWidgetValue searchField++");"
                    in  [ inertTextView searchField -- todo make function to only change hash
                        , onClick searchButton navigateAction
                        , onSubmit searchField navigateAction
                        ]
       }

instance Presentable (SearchView db) where
  present (SearchView label searchField searchButton wv script) =
      (hStretchList [E $ toHtml label +++ nbsp, Stretch $ with [style "width: 100%; background-color: red"] (present searchField), E $ present searchButton]) +++
      present wv
      +++ mkScript script

mkItemRootView = mkMaybeView "Onbekend item" $
  do { args <- getHashArgs
     ; case lookup "item" args of
         Just item | Just i <- readMaybe item -> 
           do { mItem <- withDb $ \db -> Map.lookup (ItemId i) (allItems db)
              ; case mItem of
                       Nothing    -> return Nothing
                       Just item -> fmap Just $ mkItemView Full item
              }
         Nothing -> return Nothing
      }

deriveInitial ''TestView

deriveInitial ''LendersRootView

deriveInitial ''Inline

deriveInitial ''LenderView

instance Initial LenderId where
  initial = LenderId "_uninitializedId_"

deriveInitial ''Lender

deriveInitial ''ItemsRootView

deriveInitial ''ItemView

deriveInitial ''Item

instance Initial ItemId where
  initial = ItemId (-1)
