{-# LANGUAGE DeriveDataTypeable, PatternGuards, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, TupleSections, FlexibleInstances, ScopedTypeVariables #-}
module Main where

import Data.List
import BlazeHtml
import Data.Generics
import Data.Char hiding (Space)
import Data.Function (on)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Debug.Trace
import System.Time
import Types
import ObloUtils
import Generics
import WebViewPrim
import WebViewLib
import HtmlLib
import Control.Monad.State hiding (get)
import qualified Control.Monad.State
import Server
import TemplateHaskell
import Control.Category hiding (Category) -- fclabels
import Data.Label                         -- fclabels
import Prelude hiding ((.), id)           -- fclabels

import WebViewLibExp
import Database
import LeenclubUtils

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

TODO IMPORTANT
Current event override mechanism for widgets is broken.
Say we have Button_0 which is presented and has its handlers overwritten by script code in its parents.
If we then change the presentation to a new one that has a button in the same tree position, it will also be Button_0,
and since it hasn't changed, the incrementality will not update it, causing it to have the old button's handlers.
(what happens with widget content?)


TODO: hash paths don't seem to work okay when scripts are present. Reservations example has problems when switching from main to client or restaurant views

TODO: don't prevent submit when no textfield action is present!!!! (instead change script for this or something)
otherwise we cannot override return key for textfields without action

Fix weird <div op="new"> elements appearing before <html> element

Ideas

composeView   $ \vid (a,b) -> do bla return (a,b)
instance Present (ComposeView ..)

Can't presentView simply supply a (PresentView )->Html?

mkWebViewInit (wv->wv) (wv->wv)       init arg is initial 

nice sort buttons with triangles



Home FAQ Profiel spullen geschiedenis Berichten aanmelden inloggen





-}


-- Utils


unsafeLookupM tag dbf key = withDb $ \db -> unsafeLookup tag (dbf db) key


-- Leenclub utils

showName lender = get lenderFirstName lender ++ " " ++ get lenderLastName lender



-- WebViews







data Inline = Inline | Full deriving (Eq, Show, Typeable, Data)

isInline Inline = True
isInline Full   = False

deriveInitial ''Inline

instance MapWebView Database Inline

-- TODO: maybe distance
data ItemView = 
  ItemView Inline Double Item (Maybe Item) Lender (Widget (Button Database)) (Maybe Lender) [(String,Property Database Item)] [Widget (Button Database)]
    deriving (Eq, Show, Typeable, Data)

instance Initial LenderId where
  initial = LenderId "_uninitializedId_"


deriveInitial ''Gender

deriveInitial ''Lender

instance MapWebView db Lender

instance Initial ItemId where
  initial = ItemId (-1)
      
deriveInitial ''Category

deriveInitial ''Item

instance MapWebView db Item

deriveInitial ''ItemView

deriveMapWebViewDb ''Database ''ItemView 

-- todo: use partial lense here?
mEditedItem :: ItemView :-> Maybe Item
mEditedItem = lens (\(ItemView _ _ _ mEditedItem _ _ _ _ _) -> mEditedItem)
                   (\mEditedItem (ItemView a b c d e f g h i) -> (ItemView a b c mEditedItem e f g h i))
--updateById id update db = let object = unsafeLookup 
 
mkItemView inline item = mkWebView $
  \vid oldItemView@(ItemView _ _ _ mEdited _ _ _ _ _) -> 
    do { owner <- unsafeLookupM "itemView" allLenders (get itemOwner item)
       
       ; user <- getUser
       ; button <- case (get itemBorrowed item, user) of
           (Nothing, Nothing)         -> mkButton "Leen" False $ Edit $ return () 
           (Nothing, Just (userId,_)) | get itemOwner item == LenderId userId -> mkButton "Lenen" False $ Edit $ return ()
                                      | otherwise                         -> 
             mkButton "Lenen" True  $ Edit $ docEdit $ \db -> 
               let items' = Map.update (\i -> Just $ set itemBorrowed (Just $ LenderId userId) item) (get itemId item) (allItems db) 
               in  db{allItems = items'}
           (Just borrowerId,_) -> 
             mkButton "Terug ontvangen" True  $ Edit $ docEdit $ \db -> 
               let items' = Map.update (\i -> Just $ set itemBorrowed Nothing item) (get itemId item) (allItems db) 
               in  db{allItems = items'}
           
       ; mBorrower <- maybe (return Nothing) (\borrowerId -> fmap Just $ unsafeLookupM "itemView2" allLenders borrowerId) $ get itemBorrowed item

       ; distance <- case user of
           Just (userId,_) -> do { userLender <- unsafeLookupM "itemView3" allLenders (LenderId userId)
                                 ; return $ lenderDistance userLender owner
                                 }   
           _               -> return $ -1
           
       ; props <- (if inline == Inline then getInlineCategoryProps else getFullCategoryProps) vid (isJust mEdited) item $ get itemCategory item
       
       ; buttons <- if False {- isInline inline -} then return [] else
          do { deleteButton <- mkButton "Verwijderen" True $ Edit $ docEdit $ deleteItem item
             ; editButton <- mkButton (maybe "Aanpassen" (const "Gereed") mEdited) True $
                 Edit $ case mEdited of
                          Nothing            -> viewEdit vid $ set mEditedItem (Just item)
                          Just updatedItem -> do { docEdit $ updateItem (get itemId updatedItem) $ \item -> updatedItem
                                                 ; viewEdit vid $ set mEditedItem Nothing
                                                 ; liftIO $ putStrLn $ "updating item \n" ++ show updatedItem
                                                 }
             ; buttons <- if not $ isJust mEdited then return [] else
                    fmap singleton $ mkButton "Annuleren" True $ Edit $ viewEdit vid $ set mEditedItem Nothing
             ; return $ [ deleteButton, editButton ] ++ buttons
             }
       
       
       ; return $ ItemView inline distance item mEdited owner button mBorrower props buttons
       }
       
instance Presentable ItemView where
  present (ItemView Full dist item _ owner button mBorrower props buttons) =
        vList [ h2 $ toHtml (getItemCategoryName item ++ ": " ++ get itemName item)
              , hList [ (div_ (boxedEx 1 $ image ("items/" ++ get itemImage item) ! style "height: 200px")) ! style "width: 204px" ! align "top"
                      , nbsp
                      , nbsp
                      , vList $ [ with [style "color: #333; font-size: 16px"] $
                                    presentEditableProperties -- ("Eigenaar: ", Static linkedLenderFullName owner):
                                                              props
                                ] 
                                ++ maybe [] (\borrower -> [ vSpace 10, with [style "color: red"] $ 
                                                           "Uitgeleend aan " +++ linkedLenderFullName borrower]) mBorrower
                                ++ [ vSpace 10
                                , present button ]
                      ]
              , vSpace 10
              , with [ style "font-weight: bold"] $ "Beschrijving:" 
              , multiLineStringToHtml $ get itemDescr item
              , hList $ map present buttons
              ]
  present (ItemView Inline dist item mEdited owner button mBorrower props buttons) =
    -- todo present imdb link, present movieOrSeries
      hStretchList
            [ E $ linkedItem item $ (div_ (boxedEx 1 $ image ("items/" ++ get itemImage item) ! style "height: 120px")) ! style "width: 124px" ! align "top"
            , E $  nbsp +++ nbsp
            
            -- TODO: this stretch doesn't work. Until we have good compositional layout combinators, just set the width.
            , Stretch $ linkedItem item $
                 div_ ! style "height: 120px; width: 428px; font-size: 12px" $ sequence_ 
                           [ with [style "font-weight: bold; font-size: 15px"] $ toHtml (getItemCategoryName item ++ ": " ++ get itemName item) 
                           , vSpace 2
                           , with [style "color: #333"] $
                               presentEditableProperties props -- ++
                                                   -- [("Punten", toHtml . show $ get itemPrice item)]
                           , vSpace 3
                           , with [style "font-weight: bold"] $ "Beschrijving:" 
                           , with [class_ "ellipsis multiline", style "height: 30px;"] $
                                                                  {- 30 : 2 * 14 + 2 -}
                               multiLineStringToHtml $ get itemDescr item
                           ] ! width "100%"
            , E $ nbsp +++ nbsp
            , E $  vDivList   
               ([ presentProperties $ [ ("Eigenaar", linkedLenderFullName owner)
                                      , ("Rating", with [style "font-size: 17px; position: relative; top: -6px; height: 12px" ] $ presentRating 5 $ get lenderRating owner)
                                      ] ++
                                  (if dist > 0 then [ ("Afstand", toHtml $ showDistance dist) ] else [])
                  --, div_ $ presentPrice (itemPrice item)
                ] ++
                  maybe [] (\borrower -> [with [style "color: red; font-size: 12px"] $ "Uitgeleend aan " +++ linkedLenderFullName borrower]) mBorrower
                  ++ [ vSpace 5
                , present button 
                , vSpace 10
                , hList $ map present buttons
              ]
                ) ! style "width: 200px; height: 120px; padding: 5; font-size: 12px"
            ]
vDivList elts = div_ $ mapM_ div_ elts 


presentProperties :: [(String, Html)] -> Html            
presentProperties props =
  table $ sequence_ [ tr $ sequence_ [ td $ with [style "font-weight: bold"] $ toHtml propName, td $ nbsp +++ ":" +++ nbsp
                                     , td $ toHtml propVal ] 
                    | (propName, propVal) <- props
                    ]


getItemCategoryName :: Item -> String
getItemCategoryName item = case get itemCategory item of 
                             Book{}        -> "Boek"
                             Game{}        -> "Game"
                             CD{}          -> "CD"
                             DVD{}         -> "DVD"
                             Tool{}        -> "Gereedschap"
                             Electronics{} -> "Gadget"
                             Misc{}        -> "Misc"


getInlineCategoryProps vid isEdited item c = do { props <- getAllCategoryProps vid isEdited item c
                                                ; return [ inlineProp | Left inlineProp <- props ]
                                                } 
                              
getFullCategoryProps vid isEdited item c = fmap (map $ either id id) $ getAllCategoryProps vid isEdited item c 

-- Left is only Full, Right is Full and Inline
getAllCategoryProps :: ViewId -> Bool -> Item -> Category -> WebViewM Database [Either (String,Property Database Item) (String, Property Database Item)]
getAllCategoryProps vid isEdited item c = 
  let mkStringProp leftOrRight name catField = fmap (\p -> leftOrRight (name, p)) $ mkEditableProperty vid isEdited mEditedItem (pLens "getAllCategoryProps" $ catField . itemCategory) id Just toHtml item 
      mkIntProp leftOrRight name catField = fmap (\p -> leftOrRight (name, p)) $ mkEditableProperty vid isEdited mEditedItem (pLens "getAllCategoryProps" $ catField . itemCategory) show readMaybe toHtml item
  in  sequence $ case c of
      Book{} -> 
        [ mkStringProp Left  "Auteur"        bookAuthor
        , mkIntProp    Right "Jaar"          bookYear
        , mkStringProp Left  "Taal"          bookLanguage
        , mkStringProp Left  "Genre"         bookGenre
        , mkIntProp    Right "Aantal bladz." bookPages
        ]
      Game{} ->
        [ mkStringProp Left  "Platform"  gamePlatform
        , mkIntProp    Left  "Jaar"      gameYear
        , mkStringProp Right "Developer" gameDeveloper
        , mkStringProp Left  "Genre"     gameGenre
        ]
      CD{} ->
        [ mkStringProp Left "Artiest" cdArtist
        , mkIntProp    Left "Jaar"    cdYear
        , mkStringProp Left "Genre"   cdGenre
        ]
      DVD{} ->
        [ mkIntProp    Right "Seizoen"     dvdSeason
        , mkStringProp Right "Taal"        dvdLanguage
        , mkIntProp    Right "Jaar"        dvdYear
        , mkStringProp Left "Genre"        dvdGenre
        , mkStringProp Left "Regisseur"    dvdDirector
        , mkIntProp    Right "Aantal afl." dvdNrOfEpisodes
        , mkIntProp    Right "Speelduur"   dvdRunningTime
        , fmap (\p -> Left ("IMDb", p)) $ mkEditableProperty vid isEdited mEditedItem (pLens "getAllCategoryProps" $ dvdIMDb . itemCategory) id Just 
               (\str -> if null str then "" else a (toHtml str) ! href (toValue str) ! target "_blank" ! style "color: blue") 
               item -- this one is special because of the html presentation as a link
        ]
      Tool{} ->
        [ mkStringProp Left "Merk" toolBrand
        , mkStringProp Left "Type" toolType
        ]
      _ -> []

presentPrice price =
  with [style "width:30px; height:28px; padding: 2 0 0 0; color: white; background-color: black; font-family: arial; font-size:24px; text-align: center"] $
    toHtml $ show price 

instance Storeable Database ItemView

linkedItemName item = linkedItem item $ toHtml (get itemName item)

linkedItem item html = 
  a ! (href $ (toValue $ "/#item&item=" ++ (show $ get (itemIdNr . itemId) item))) << html

linkedLenderName lender = linkedLender lender $ toHtml $ get (lenderIdLogin . lenderId) lender

linkedLenderFullName lender = linkedLender lender $ toHtml (get lenderFirstName lender ++ " " ++ get lenderLastName lender)
  
linkedLender lender html = 
  a! (href $ (toValue $ "/#lener&lener=" ++ get (lenderIdLogin . lenderId) lender)) << html

rootViewLink :: String -> Html -> Html 
rootViewLink rootViewName html = a ! (href $ (toValue $ "/#" ++ rootViewName)) << html



data LeenclubLoginOutView = LeenclubLoginOutView (WebView Database) deriving (Eq, Show, Typeable, Data)

deriveInitial ''LeenclubLoginOutView

deriveMapWebViewDb ''Database ''LeenclubLoginOutView

instance Storeable Database LeenclubLoginOutView

mkLeenClubLoginOutView = mkWebView $
  \vid oldItemView@LeenclubLoginOutView{} ->
   do { user <- getUser
      ; loginOutView <- if user == Nothing then mkLoginView 
                                           else mkLogoutView
      ; return $ LeenclubLoginOutView loginOutView
      }
       
instance Presentable LeenclubLoginOutView where
  present (LeenclubLoginOutView loginOutView) =
    present loginOutView



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
      

data LenderView = 
  LenderView Inline User Lender (Maybe Lender) {- [Property Lender] -} [(String,Property Database Lender)]  [(String,Property Database Lender)]
             --(Maybe (Widget (TextView Database, TextView Database)))
             [WebView Database] [Widget (Button Database)]
    deriving (Eq, Show, Typeable, Data)

 -- todo: edit button in Inline/Full datatype?                  

deriveInitial ''LenderView

deriveMapWebViewDb ''Database ''LenderView 

mEditedLender :: LenderView :-> Maybe Lender
mEditedLender = lens (\(LenderView _ _ _ mEditedLender _ _ _ _) -> mEditedLender)
                     (\mLender (LenderView a b c d e f g h) -> (LenderView a b c mLender e f g h))

instance Storeable Database LenderView -- where
--  save (LenderView _ _ _ modifiedLender@Lender{lenderId=lId} _ _  _ _)  = updateLender lId $ \lender -> modifiedLender

mkLenderView inline lender = mkWebView $
  \vid oldLenderView@(LenderView _ _ _ mEdited _ _ _ _) ->
    do { mUser <- getUser
       ; let itemIds = get lenderItems lender
       ; items <- withDb $ \db -> getOwnedItems (get lenderId lender) db

       ; fName <- mkTextField (get lenderFirstName lender)
       ; lName <- mkTextField (get lenderLastName lender)

{-
       ; prop0 <- mkStaticProperty (lenderIdLogin . lenderId) toHtml lender
       ; prop1 <- mkEditableProperty vid (isJust mEdited) mEditedLender lenderFirstName id Just toHtml lender
       ; let testProps = [ prop0, prop1 ]
-}
       ; props <- (if lenderIsUser lender mUser then getLenderPropsSelf else getLenderPropsEveryone) vid (isJust mEdited) lender
       ; extraProps <- if lenderIsUser lender mUser then getExtraProps vid (isJust mEdited) lender else return [] 
       ; (itemWebViews, buttons) <- if isInline inline then return ([],[]) else
          do { itemWebViews <-  mapM (mkItemView Inline) items
             ; editButton <- mkButton (maybe "Aanpassen" (const "Gereed") mEdited) (lenderIsUser lender mUser) $
                 Edit $ case mEdited of
                          Nothing            -> viewEdit vid $ set mEditedLender (Just lender)
                          Just updatedLender -> do { docEdit $ updateLender (get lenderId updatedLender) $ \lender -> updatedLender
                                                   ; viewEdit vid $ set mEditedLender Nothing
                                                   ; liftIO $ putStrLn $ "updating lender\n" ++ show updatedLender
                                                   }
             ; buttons <- if not $ isJust mEdited then return [] else
                    fmap singleton $ mkButton "Annuleren" True $ Edit $ viewEdit vid $ set mEditedLender Nothing
             ; return (itemWebViews, [ editButton ] ++ buttons)
             }
       ; return $ LenderView inline mUser lender mEdited {- testProps -} props extraProps itemWebViews buttons 
       }
       
lenderIsUser lender Nothing          = False
lenderIsUser lender (Just (login,_)) = get (lenderIdLogin . lenderId) lender == login
 
instance Presentable LenderView where
  present (LenderView Full mUser lender _ {- testProps -} props extraProps itemWebViews buttons)   =
        vList [ vSpace 20
              , hList [ (div_ (boxedEx 1 $ image ("leners/" ++ get lenderImage lender) ! style "height: 200px")) ! style "width: 204px" ! align "top"
                      , hSpace 20
                      , vList [ h2 $ {- if editing 
                                     then hList [ present fName, nbsp, present lName ] 
                                     else -} (toHtml $ showName lender) -- +++ (with [style "display: none"] $ concatHtml $ map present [fName,lName]) -- todo: not nice!
                              , hList [ vList [ presentEditableProperties props
                                              --, vList $ map present testProps
                                              , vSpace 20
                                              , hList $ map present buttons
                                              ]
                                      , hSpace 20
                                      , presentEditableProperties extraProps
                                      ]
                              ]
                      ]
              , vSpace 20
              , h2 $ (toHtml $ "Spullen van "++get lenderFirstName lender)
              , vList $ map present itemWebViews
              ]
  present (LenderView Inline mUser lender mEdited {- testProps -} props extraProps itemWebViews buttons) =
    linkedLender lender $
      hList [ (div_ (boxedEx 1 $ image ("leners/" ++ get lenderImage lender) ! style "height: 30px")) ! style "width: 34px" ! align "top"
            , nbsp
            , nbsp
            , vList [ toHtml (showName lender)
                    --, vList $ map present testProps
                    , span_ (presentRating 5 $ get lenderRating lender) ! style "font-size: 20px"
                    , hList $ map present buttons
                                              
                    ]
         --   , with [style "display: none"] $ concatHtml $ map present [fName,lName] ++ [present editButton] -- todo: not nice! 
            ]

                           
getLenderPropsEveryone vid isEdited lender = do { props <- getLenderPropsAll vid isEdited lender
                                                ; return [ prop | Left prop <- props ]
                                                }
getLenderPropsSelf vid isEdited lender = do { props <- getLenderPropsAll vid isEdited lender
                                            ; return [ either id id eProp  | eProp <- props ]
                                            }
   -- todo: composed properties? address is street + nr
   --       non-string properties?                        
getLenderPropsAll vid isEdited lender = sequence
  [ fmap (\p -> Right ("LeenClub ID", p)) $ mkStaticProperty (lenderIdLogin . lenderId) toHtml lender
  , fmap (\p -> Left ("M/V", p)) $ mkEditableSelectProperty vid isEdited mEditedLender lenderGender show (toHtml . show) [M,F] lender
  , fmap (\p -> Left ("E-mail", p)) $ mkEditableProperty vid isEdited mEditedLender lenderMail id Just toHtml lender
  , fmap (\p -> Right ("Adres", p)) $ mkEditableProperty vid isEdited mEditedLender lenderStreet id Just toHtml lender
  , fmap (\p -> Left ("Postcode", p)) $ mkEditableProperty vid isEdited mEditedLender lenderZipCode id Just toHtml lender 
  , fmap (\p -> Right ("Woonplaats", p)) $ mkEditableProperty vid isEdited mEditedLender lenderCity id Just toHtml lender 
  ]

getExtraProps' lender = [ ("Rating", with [style "font-size: 20px; position: relative; top: -5px; height: 17px" ] (presentRating 5 $ get lenderRating lender)) 
                       , ("Puntenbalans", toHtml . show $ get lenderNrOfPoints lender)
                       , ("Aantal spullen", toHtml . show $ length (get lenderItems lender))
                       ]
getExtraProps vid isEdited lender = sequence 
  [ fmap ("Rating",) $ mkEditableSelectProperty vid isEdited mEditedLender lenderRating show (presentRating 5) [0..5] lender 
  , fmap ("Puntenbalans",) $ mkStaticProperty lenderNrOfPoints (toHtml . show) lender
  , fmap ("Aantal spullen",) $ mkStaticProperty lenderItems (toHtml . show  . length ) lender
  ]

{-
 Lender { lenderId :: LenderId, lenderFirstName :: String, lenderLastName :: String, lenderGender :: Gender 
         , lenderMail :: String
         , lenderStreet :: String, lenderStreetNr :: String, lenderCity :: String, lenderZipCode :: String
         , lenderCoords :: (Double, Double) -- http://maps.google.com/maps/geo?q=adres&output=xml for lat/long
         , lenderImage :: String
         , lenderRating :: Int, lenderItems :: [ItemId]
-}
data ItemsRootView = 
  ItemsRootView (WebView Database) (WebView Database) (Widget (Button Database))
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''ItemsRootView

deriveMapWebViewDb ''Database ''ItemsRootView

instance Storeable Database ItemsRootView

mkItemsRootView ::WebViewM Database (WebView Database)
mkItemsRootView = mkWebView $
  \vid oldLenderView@(ItemsRootView _ _ _) ->
    do { let namedSortFunctions = [ ("Naam",     compare `on` get itemName) 
                                  , ("Prijs",    compare `on` get itemPrice)
                                  , ("Eigenaar", compare `on` get itemDescr)
                                  ]
    
       ; searchView <- mkSearchView "Zoek in spullen:" "q" $ \searchTerm ->
          do { results :: [Item] <- withDb $ \db -> searchItems searchTerm db 
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
        present searchView >> present dialogButton >> present dialog


    
data LendersRootView = LendersRootView (WebView Database)
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''LendersRootView

deriveMapWebViewDb ''Database ''LendersRootView

instance Storeable Database LendersRootView

mkLendersRootView :: WebViewM Database (WebView Database)
mkLendersRootView = mkWebView $
  \vid oldLenderView@(LendersRootView _) ->
    do { let namedSortFunctions = [ ("Voornaam",   compare `on` get lenderFirstName) 
                                  , ("Achternaam", compare `on` get lenderLastName) 
                                  , ("Rating",     compare `on` get lenderRating)
                                  ]
    
       ; searchView <- mkSearchView "Zoek in leners: " "q" $ \searchTerm ->
          do { results :: [Lender] <- withDb $ \db -> searchLenders searchTerm db
             ; mkSortView namedSortFunctions (mkLenderView Inline) results
             }
       ; return $ LendersRootView searchView
       }

instance Presentable LendersRootView where
  present (LendersRootView searchView) = present searchView


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



data BorrowedRootView = BorrowedRootView [WebView Database]  [WebView Database]
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''BorrowedRootView

instance Storeable Database BorrowedRootView

mkBorrowedRootView :: WebViewM Database (WebView Database)
mkBorrowedRootView = mkWebView $
  \vid oldLenderView@(BorrowedRootView _ _) ->
   do { Just (login,_) <- getUser
      ; borrowedItems <- withDb $ \db -> getBorrowedItems (LenderId login) db
      ; borrowed <- mapM (mkItemView Inline) borrowedItems
      ; lendedItems <- withDb $ \db -> getLendedItems (LenderId login) db
      ; lended <- mapM (mkItemView Inline) lendedItems
      ; return $ BorrowedRootView borrowed lended
      }

instance Presentable BorrowedRootView where
  present (BorrowedRootView borrowed lended) =
    h3 "Geleend" +++
    vList (map present borrowed) +++
    h3 "Uitgeleend" +++ 
    vList (map present lended)

-- unnecessary at the moment, as the page has no controls of its own
data LeenclubPageView = LeenclubPageView User String (Widget (EditAction Database)) (WebView Database) deriving (Eq, Show, Typeable, Data)

deriveInitial ''LeenclubPageView

deriveMapWebViewDb ''Database ''LeenclubPageView

instance Storeable Database LeenclubPageView

--updateById id update db = let object = unsafeLookup 

mkLeenclubPageView menuItemLabel mWebViewM = mkWebView $
  \vid oldItemView@LeenclubPageView{} ->
   do { user <- getUser
      ; wv <- mWebViewM
      ; logoutAction <- mkEditAction LogoutEdit
      ; return $ LeenclubPageView user menuItemLabel logoutAction wv
      } 

-- TODO: click in padding does not select item
instance Presentable LeenclubPageView where
  present (LeenclubPageView user menuItemLabel logoutAction wv) =
    -- imdb: background-color: #E3E2DD; background-image: -moz-linear-gradient(50% 0%, #B3B3B0 0px, #E3E2DD 500px);  
    mkPage [thestyle $ gradientStyle (Just 500) "#444" {- "#B3B3B0" -} "#E3E2DD"  ++ " font-family: arial"] $ 
      vList [ with [style "font-size: 50px; color: #ddd"] "Leenclub.nl"
            --, present loginOutView
            , div_ ! thestyle "border: 1px solid black; background-color: #f0f0f0; box-shadow: 0 0 8px rgba(0, 0, 0, 0.7);" $ 
                vList [ hStretchList (map (E . highlightItem) leftMenuItems ++ [space] ++ map (E . highlightItem) rightMenuItems)
                         ! (thestyle $ "color: white; font-size: 17px;"++ gradientStyle Nothing "#707070" "#101010")
                      , div_ ! thestyle "padding: 10px" $ present wv ] ! width "800px"
            ]
   where leftMenuItems = (map (\(label,rootView) -> (label, rootViewLink rootView $ toHtml label)) $
                        [("Home",""), ("Leners", "leners"), ("Spullen", "items")] ++ userMenuItems user)
         rightMenuItems = [ if user == Nothing then ("Login", rootViewLink "login" "Login") 
                                               else ("Logout", withEditAction logoutAction "Logout") ] -- Logout is not menu, so it will not be highlighted
         userMenuItems Nothing = []
         userMenuItems (Just (userId, _)) = [("Mijn profiel", "lener&lener="++userId), ("Geleend", "geleend")]
         
         highlightItem (label, e) = with [ onmouseover "this.style.backgroundColor='#666'" -- not nice, but it works and prevents
                                         , onmouseout  "this.style.backgroundColor=''"     -- the need for a css declaration
                                         , thestyle $ "height: 25px; margin: 0 20 0 20; " ++ 
                                                    if label == menuItemLabel 
                                                    then gradientStyle Nothing "#303030" "#101010" 
                                                    else "" ] $
                                    with [style "padding: 2 10 5 10;" ] e
                                     
gradientStyle :: Maybe Int -> String -> String -> String
gradientStyle mHeight topColor bottomColor =
    "background: -moz-linear-gradient("++topColor++" 0px, "++bottomColor++ maybe "" (\h -> " "++show h++"px") mHeight ++ "); "
  ++"background: -webkit-gradient(linear, left top, left "++maybe "bottom" show mHeight ++", from("++topColor++"), to("++bottomColor++"));"
  
  
  
  


mkHomeView :: WebViewM Database (WebView Database)
mkHomeView = mkHtmlTemplateView "LeenclubWelcome.html" []

--- Testing


data TestView = 
  TestView Int (Widget (RadioView Database)) (Widget (Button Database)) (Widget (TextView Database)) (WebView Database) (WebView Database)
           (Widget (LabelView Database)) (Widget (TextView Database))
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''TestView

deriveMapWebViewDb ''Database ''TestView

mkTestView :: WebViewM Database (WebView Database)
mkTestView = mkWebView $
  \vid oldTestView@(TestView _ radioOld _ _ _ _ _ oldTxtArea) ->
    do { radio <-  mkRadioViewWithChange ["Naam", "Punten", "Drie"] (getSelection radioOld) True $ \sel -> Edit $ viewEdit vid $ \v -> trace ("selected"++show sel) v :: TestView
       ; let radioSel = getSelection radioOld
       ; b <- mkButton "Test button" True $ Edit $ viewEdit vid $ \(TestView a b c d e f g h) -> TestView 2 b c d e f g h
       ; tf <- mkTextField "bla"
       ; liftIO $ putStr $ show oldTestView
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
       
       ; lbl <- mkLabelViewWithStyle "label" "color: blue"
       ; txtArea <- mkTextAreaWithStyleChange (getStrVal oldTxtArea) ("background-color: "++if getStrVal oldTxtArea /= "" then "green" else "red") $ \str -> Edit $ viewEdit vid $ \v -> trace ("edited "++show str) v :: TestView
       ; let wv = TestView radioSel radio b tf wv1' wv2' lbl txtArea
       
       
       --; liftIO $ putStrLn $ "All top-level webnodes "++(show (everythingTopLevel webNodeQ wv :: [WebNode Database])) 
       
       ; return $ wv 
       }

instance Presentable TestView where
  present (TestView radioSel radio button tf wv1 wv2 lbl txtArea) =
      vList [present radio, present button, present tf, toHtml $ show radioSel, present wv1, present wv2
            , present lbl
            , present txtArea
            ]


instance Storeable Database TestView



data TestView2 = 
  TestView2 (Widget (EditAction Database)) (Widget (RadioView Database)) (Widget (TextView Database))
           String String 
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''TestView2

deriveMapWebViewDb ''Database ''TestView2

mkTestView2 :: WebViewM Database (WebView Database)
mkTestView2 = mkWebView $
  \vid oldTestView@(TestView2 ea radioOld text str1 str2) ->
    do { ea <- mkEditAction $ Edit $ viewEdit vid $ \(TestView2 a b c d e) -> TestView2 a b c "clicked" e
       ; radio <-  mkRadioViewWithStyle ["Edit", "View"] (getSelection radioOld) True "background-color: red"
       ; let radioSel = getSelection radioOld
       ; text <- mkTextFieldWithStyle "Test" "background-color: red" `withTextViewChange` (\str -> Edit $ viewEdit vid $ \(TestView2 a b c d e) -> TestView2 a b c d str)
       ; liftIO $ putStr $ show oldTestView
       ; liftIO $ putStrLn $ "radio value " ++ show radioSel
     --  ; propV2 <- mkPropertyView (radioSel == 0) "Straat" str2 $ \str -> viewEdit vid $ \(TestView2 a b c d) -> TestView2 a b  c str
       ; let wv = TestView2 ea radio text str1 str2
       
       --; liftIO $ putStrLn $ "All top-level webnodes "++(show (everythingTopLevel webNodeQ wv :: [WebNode Database])) 
       
       ; return $ wv 
       }

instance Presentable TestView2 where
  present (TestView2 ea radio text p1str p2str) =
      vList [ present radio
            , present text
            , toHtml $ "Property strings: " ++ show p1str ++ " and " ++ show p2str
            , withEditAction ea "click me"
            ]

instance Storeable Database TestView2


mkTestView3 msg = mkPresentView (\hs -> hList $ toHtml (msg :: String) : hs) $
    do { wv1 <- mkHtmlView $ "een"
       ; wv2 <- mkHtmlTemplateView "test.html" []
       ; return $ [wv1,wv2] 
       }


-- some webviews for testing with ghci

data AView db = AView (WebView db) (Widget (TextView db)) String (Widget (TextView db))
              | AAView (WebView db)


{-
instance MapWebView db (AView db) where
  mapWebView (AView wv1 wd1 str wd2) = 
    AView <$> mapWebView wv1 <*> mapWebView wd1 <*> mapWebView str <*> mapWebView wd2 
-}
deriveMapWebView ''AView

data BView db = BView String (AView db)

instance MapWebView db (BView db) where
  mapWebView (BView str a) = BView <$> mapWebView str <*> mapWebView a

--testmkwv :: x -> WebView Database
testmkwv x = WebView (ViewId []) noId noId undefined $ x 

testwv :: Int -> WebView Database
testwv i = testmkwv $ HtmlTemplateView (show i)
 
testwv0 :: WebView Database
testwv0 =  WebView (ViewId []) (Id 1) (Id 2) undefined $ ItemsRootView (testwv 1) (testwv 1) $
                   buttonWidget (ViewId []) "click me" True "" "" LogoutEdit

testwd :: String -> Widget (Button Database)
testwd str = buttonWidget (ViewId []) str True "" "" LogoutEdit
testproplist :: [(String, Property Database Item)]
testproplist =  [("LeenClub ID",StaticProperty "martijn"),("M/V",EditableProperty (Right (PropertySelectView (Widget {getWidgetStubId = Id {unId = -1}, getWidgetId = Id {unId = -1}, getWidgetWidget = SelectView {getSelectViewId = ViewId [], getSelectItems = ["M","F"], getSelectSelection = 0, getSelectEnabled = True, getSelectStyle = "", getSelectChange = Just undefined}}))))]
deriveMapWebViewDb ''Database ''BorrowedRootView




---- Main (needs to be below all webviews that use deriveInitial)

main :: IO ()
main = server rootViews "LeenclubDB.txt" mkInitialDatabase users

rootViews :: RootViews Database
rootViews = [ ("",       mkLeenclubPageView "Home"   mkHomeView), ("test", mkTestView), ("test2", mkTestView2), ("test3", mkTestView3 "msg")
            , ("leners", mkLeenclubPageView "Leners" mkLendersRootView), ("lener", mkLeenclubPageView "Lener" mkLenderRootView)
            , ("items",  mkLeenclubPageView "Spullen" mkItemsRootView),   ("item",  mkLeenclubPageView "Item"  mkItemRootView) 
            , ("geleend", mkLeenclubPageView "Geleend" mkBorrowedRootView)
            , ("login",  mkLeenclubPageView "Login"  mkLeenClubLoginOutView)
            ] 
