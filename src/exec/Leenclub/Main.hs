{-# LANGUAGE DeriveDataTypeable, PatternGuards, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell, TupleSections, FlexibleInstances, ScopedTypeVariables #-}
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
import TemplateHaskell

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

-- todo: different languages 
mkSearchView label argName resultsf = mkWebView $
  \vid oldView@( SearchView _ _ _ _ _) ->
    do { args <- getHashArgs
       ; let searchTerm = case lookup argName args of 
                            Nothing    -> ""
                            Just term -> term 
       ; searchField <- mkTextField searchTerm `withTextViewSubmit` Edit $ return () 
       ; searchButton <- mkButtonWithClick "Zoek" True $ const ""
       ; results <- resultsf searchTerm
       ; return $ SearchView label searchField searchButton results $
                  jsScript $
                    let navigateAction = "setHashArg('"++argName++"', "++jsGetWidgetValue searchField++");"
                    in  [ inertTextView searchField
                        , onClick searchButton navigateAction
                        , onSubmit searchField navigateAction
                        ]
       }

instance Presentable (SearchView db) where
  present (SearchView label searchField searchButton wv script) =
      (hStretchList [E $ toHtml label +++ nbsp, Stretch $ with [style "width: 100%;"] (present searchField), E $ present searchButton]) +++
      present wv
      +++ mkScript script


-- Leenclub utils

showName lender = lenderFirstName lender ++ " " ++ lenderLastName lender



-- WebViews




data Inline = Inline | Full deriving (Eq, Show, Typeable, Data)

isInline Inline = True
isInline Full   = False

deriveInitial ''Inline


-- TODO: maybe distance
data ItemView = 
  ItemView Inline Double Item Lender (Widget (Button Database)) (Maybe Lender)
    deriving (Eq, Show, Typeable, Data)

instance Initial LenderId where
  initial = LenderId "_uninitializedId_"


deriveInitial ''Gender

deriveInitial ''Lender

instance Initial ItemId where
  initial = ItemId (-1)
      
deriveInitial ''Category

deriveInitial ''Item

deriveInitial ''ItemView

--updateById id update db = let object = unsafeLookup 
 
mkItemView inline item = mkWebView $
  \vid oldItemView@ItemView{} -> 
    do { owner <- unsafeLookupM "itemView" allLenders (itemOwner item)
       
       ; user <- getUser
       ; button <- case (itemBorrowed item, user) of
           (Nothing, Nothing)         -> mkButton "Leen" False $ Edit $ return () 
           (Nothing, Just (userId,_)) | itemOwner item == LenderId userId -> mkButton "Lenen" False $ Edit $ return ()
                                      | otherwise                         -> 
             mkButton "Lenen" True  $ Edit $ docEdit $ \db -> 
               let items' = Map.update (\i -> Just $ item{itemBorrowed = Just $ LenderId userId}) (itemId item) (allItems db) 
               in  db{allItems = items'}
           (Just borrowerId,_) -> 
             mkButton "Terug ontvangen" True  $ Edit $ docEdit $ \db -> 
               let items' = Map.update (\i -> Just $ item{itemBorrowed = Nothing}) (itemId item) (allItems db) 
               in  db{allItems = items'}
           
       ; mBorrower <- maybe (return Nothing) (\borrowerId -> fmap Just $ unsafeLookupM "itemView2" allLenders borrowerId) $ itemBorrowed item

       ; distance <- case user of
           Just (userId,_) -> do { userLender <- unsafeLookupM "itemView3" allLenders (LenderId userId)
                                 ; return $ lenderDistance userLender owner
                                 }   
           _               -> return $ -1
       ; return $ ItemView inline distance item owner button mBorrower
       }
       
instance Presentable ItemView where
  present (ItemView Full dist item owner button mBorrower) =
        vList [ h2 $ toHtml (getItemCategoryName item ++ ": " ++ itemName item)
              , hList [ (div_ (boxedEx 1 $ image ("items/" ++ itemImage item) ! style "height: 200px")) ! style "width: 204px" ! align "top"
                      , nbsp
                      , nbsp
                      , vList $ [ with [style "color: #333; font-size: 16px"] $
                                    presentProperties $ ("Eigenaar: ", linkedLenderFullName owner):
                                                        (map (\(p,v)->(p, toHtml v)) $ getFullCategoryProps $ itemCategory item)
                                ] 
                                ++ maybe [] (\borrower -> [ vSpace 10, with [style "color: red"] $ 
                                                           "Uitgeleend aan " +++ linkedLenderFullName borrower]) mBorrower
                                ++ [ vSpace 10
                                , present button ]
                      ]
              , vSpace 10
              , with [ style "font-weight: bold"] $ "Beschrijving:" 
              , multiLineStringToHtml $ itemDescr item
              ]
  present (ItemView Inline dist item owner button mBorrower) =
    -- todo present imdb link, present movieOrSeries
      hStretchList
            [ E $ linkedItem item $ (div_ (boxedEx 1 $ image ("items/" ++ itemImage item) ! style "height: 120px")) ! style "width: 124px" ! align "top"
            , E $  nbsp +++ nbsp
            
            -- TODO: this stretch doesn't work. Until we have good compositional layout combinators, just set the width.
            , Stretch $ linkedItem item $
                 div_ ! style "height: 120px; width: 428px; font-size: 12px" $ sequence_ 
                           [ with [style "font-weight: bold; font-size: 15px"] $ toHtml (getItemCategoryName item ++ ": " ++ itemName item) 
                           , vSpace 2
                           , with [style "color: #333"] $
                               presentProperties $ (getInlineCategoryProps $ itemCategory item) ++
                                                   [("Punten", toHtml . show $ itemPrice item)]
                           , vSpace 3
                           , with [style "font-weight: bold"] $ "Beschrijving:" 
                           , with [class_ "ellipsis multiline", style "height: 30px;"] $
                                                                  {- 30 : 2 * 14 + 2 -}
                               multiLineStringToHtml $ itemDescr item
                           ] ! width "100%"
            , E $ nbsp +++ nbsp
            , E $  vDivList   
               ([ presentProperties $ [ ("Eigenaar", linkedLenderFullName owner)
                                      , ("Rating", with [style "font-size: 17px; position: relative; top: -6px; height: 12px" ] $ presentRating 5 $ lenderRating owner)
                                      ] ++
                                  (if dist > 0 then [ ("Afstand", toHtml $ showDistance dist) ] else [])
                  --, div_ $ presentPrice (itemPrice item)
                ] ++
                  maybe [] (\borrower -> [with [style "color: red; font-size: 12px"] $ "Uitgeleend aan " +++ linkedLenderFullName borrower]) mBorrower
                  ++ [ vSpace 5
                , present button ]
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
getItemCategoryName Item{itemCategory=Book{}}        = "Boek"
getItemCategoryName Item{itemCategory=Game{}}        = "Game"
getItemCategoryName Item{itemCategory=CD{}}          = "CD"
getItemCategoryName Item{itemCategory=DVD{}}         = "DVD"
getItemCategoryName Item{itemCategory=Tool{}}        = "Gereedschap"
getItemCategoryName Item{itemCategory=Electronics{}} = "Gadget"
getItemCategoryName Item{itemCategory=Misc{}}        = "Misc"

getInlineCategoryProps c = [ inlineProp | Left inlineProp <- getAllCategoryProps c ] 
getFullCategoryProps c = [ either id id eProp  | eProp <- getAllCategoryProps c ] 

-- Left is only Full, Right is Full and Inline
getAllCategoryProps :: Category -> [Either (String,Html) (String,Html)]
getAllCategoryProps c@Book{}        = [ Left ("Auteur", toHtml $ bookAuthor c), Right ("Jaar", toHtml . show $ bookYear c), Left ("Taal", toHtml $ bookLanguage c), Left ("Genre", toHtml $ bookGenre c), Right ("Aantal bladz.", toHtml . show $ bookPages c) ]
getAllCategoryProps c@Game{}        = [ Left ("Platform", toHtml $ gamePlatform c), Left ("Jaar", toHtml . show $ gameYear c), Right ("Developer", toHtml $ gameDeveloper c), Right ("Genre", toHtml $ gameGenre c) ]
getAllCategoryProps c@CD{}          = [ Left ("Artiest", toHtml $ cdArtist c), Left ("Jaar", toHtml . show $ cdYear c), Left ("Genre", toHtml $ cdGenre c) ]
getAllCategoryProps c@DVD{}         = [ Right ("Seizoen", toHtml . show $ dvdSeason c)
                                      , Right ("Taal", toHtml $ dvdLanguage c)
                                      , Right ("Jaar", toHtml . show $ dvdYear c)
                                      , Left ("Genre", toHtml $ dvdGenre c)
                                      , Left ("Regisseur", toHtml $ dvdDirector c)
                                      , Right ("Aantal afl.", toHtml . show $ dvdNrOfEpisodes c)
                                      , Right ("Speelduur", toHtml . show $ dvdRunningTime c)
                                      , Left ("IMdb", if null $ dvdIMDb c then "" else a (toHtml $ dvdIMDb c) ! href (toValue $ dvdIMDb c) ! target "_blank" ! style "color: blue")
                                      ]
getAllCategoryProps c@Tool{}        = [ Left ("Merk", toHtml $ toolBrand c), Left ("Type", toHtml $ toolType c) ]
getAllCategoryProps c@Electronics{} = []
getAllCategoryProps c@Misc{}        = [] 

                    
presentPrice price =
  with [style "width:30px; height:28px; padding: 2 0 0 0; color: white; background-color: black; font-family: arial; font-size:24px; text-align: center"] $
    toHtml $ show price 

instance Storeable Database ItemView

linkedItemName item@Item{itemId = ItemId i} = linkedItem item $ toHtml (itemName item)

linkedItem item@Item{itemId = ItemId login} html = 
  a ! (href $ (toValue $ "/#item&item=" ++ (show login))) << html

linkedLenderName lender@Lender{lenderId = LenderId login} = linkedLender lender $ toHtml login

linkedLenderFullName lender = linkedLender lender $ toHtml (lenderFirstName lender ++ " " ++ lenderLastName lender)
  
linkedLender lender@Lender{lenderId = LenderId login} html = 
  a! (href $ (toValue $ "/#lener&lener=" ++ login)) << html

rootViewLink :: String -> Html -> Html 
rootViewLink rootViewName html = a ! (href $ (toValue $ "/#" ++ rootViewName)) << html



data LeenclubLoginOutView = LeenclubLoginOutView (WebView Database) deriving (Eq, Show, Typeable, Data)

deriveInitial ''LeenclubLoginOutView

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
      
      
-- An encapsulated database update that can be part of a webview. 
data DBUpdate = DBUpdate (String -> Database -> Database) deriving (Typeable, Data)


instance Initial DBUpdate where
  initial = DBUpdate $ const id
  
-- never equal, so no incrementality
instance Eq DBUpdate where
  _ == _ = False
  
instance Show DBUpdate where
  show _ = "DBUpdate"

--instance Typeable DBUpdate
{-
instance Data DBUpdate where
  gfoldl k z (DBUpdate f) = z DBUpdate `k` f
     
  gunfold k z c = error "gunfold not defined for DBUpdate"
     
  toConstr (DBUpdate _) = con_DBUpdate 
  dataTypeOf _ = ty_DBUpdate
-}
ty_DBUpdate = mkDataType "Main.DBUpdate" [con_DBUpdate]
con_DBUpdate = mkConstr ty_WebView "DBUpdate" [] Prefix
--

-- non-optimal way to show editable properties. The problem is that the update specified is not a view update but a database update.
data Property = Property String DBUpdate (Either String (Widget (TextView Database)))
                  deriving (Eq, Show, Typeable, Data)


deriveInitial ''Property


{- The update is a database update, but it would be better to be able to specify a view update, since we don't 
want to commit all textfields immediately to the database. Maybe save could be part of the edit monad? (but then do we need
to save again after performing the viewEdit in save?) or we could add an edit action to text fields (not the commit action, but
a blur action) -}
mkPropertyView :: Bool -> String -> String -> (String -> Database -> Database) ->  WebViewM Database (WebView Database)
mkPropertyView False name val update = mkWebView $
  \vid _ ->
   do { return $ Property name (DBUpdate update) (Left val)
      }
mkPropertyView True name val update = mkWebView $
  \vid propV ->
   do { valV <- mkTextField val 
        {- rec { valV <- mkTextFieldAct val $ Edit $ do { (sessionId, user, db, rootView, pendingEdit, hashArgs) <- get
                                                     ; str <- getTextViewContents valV
                                                     ; liftIO $ putStrLn $ "RootView\n" ++ show rootView
                                                     ; liftIO $ putStrLn $ "Str is "++str
                                                     ; update str
                                                     } 
            } -} 
      ; return $ Property name (DBUpdate update)  (Right valV)
      }
      
instance Presentable Property where
  present (Property name _ (Left val))   = toHtml name +++ toHtml val
  present (Property name _ (Right valV)) = toHtml name +++ present valV

instance Storeable Database Property where
  save (Property _ _ (Left _))       db = db
  save (Property name (DBUpdate upd) (Right valV)) db = upd (getStrVal valV) db 

-- use view updates so we can apply a list of properties to the view
newtype ParseFunction a = ParseFunction (String -> Maybe a) deriving (Typeable, Data)

instance Show (ParseFunction a) where
  show _ = "<ParseFunction>"

instance Eq (ParseFunction a) where
  _ ==  _ = False

-- Second non-optimal way to show editable properties. Properties can get a view update (which is not implemented yet),
-- but it is awkward to handle initial property values (see comment horrible in mkLenderView)
data Property2 a = Property2 a (ParseFunction a) (Either String (Widget (TextView Database)))
                  deriving (Eq, Show, Typeable, Data)

-- not a web view, but it is an instance of Presentable
mkProperty2 :: Bool -> (a -> String) -> (String -> Maybe a) -> a -> WebViewM Database (Property2 a)
mkProperty2 editing present parse value =
 do { eValue <- if editing
                then fmap Right $ mkTextField (present value)
                else return $ Left (present value) 
    ; return $ Property2 value (ParseFunction parse) eValue
    }

instance Presentable (Property2 a) where
  present (Property2 _ _ (Left str)) = toHtml str
  present (Property2 _ _ (Right textField)) = present textField
  
getPropertyValue :: Property2 a -> Maybe a
getPropertyValue (Property2 value _ (Left _)) = Just value
getPropertyValue (Property2 value (ParseFunction parse) (Right textField)) = parse $ getStrVal textField

data LenderView = 
  LenderView Inline Bool User Lender  [WebView Database] [Property2 String]
             --(Maybe (Widget (TextView Database, TextView Database)))
             [WebView Database] (Widget (Button Database))
    deriving (Eq, Show, Typeable, Data)

                   


deriveInitial ''LenderView


instance Storeable Database LenderView where
  save (LenderView _ _ _ modifiedLender@Lender{lenderId=lId} _ _  _ _)  = updateLender lId $ \lender -> modifiedLender


mkLenderView inline lender = mkWebView $
  \vid oldLenderView@(LenderView _ editing _ _ _ oldProps' _ _) ->
    do { mUser <- getUser
       ; let itemIds = lenderItems lender
       ; items <- withDb $ \db -> getOwnedItems (lenderId lender) db

       ; fName <- mkTextField (lenderFirstName lender)
       ; lName <- mkTextField (lenderLastName lender)
       
       ; prop1 <- mkPropertyView editing "E-mail" (lenderMail lender) $ \str -> trace ("updating lendermail "++str) $ 
                                                                                updateLender (lenderId lender) (\l -> l{lenderMail = str})
       ; let props = [prop1]

       ; liftIO $ putStrLn "initializing prop1'"
       
       -- this is horrible
       ; prop1' <- mkProperty2 editing id Just $ case oldProps' of [oldProp'] | Just v <- getPropertyValue oldProp' -> v
                                                                   _ -> lenderZipCode lender
       ; let props' = [prop1']
       
       ; itemWebViews <- if isInline inline then return [] else mapM (mkItemView Inline) items
       ; editButton <- mkButton (if editing then "Gereed" else "Aanpassen") (lenderIsUser lender mUser) $ 
           Edit $ viewEdit vid $ (\lv@(LenderView a editing b lender d [prop'] f g) ->
                                   if editing 
                                   then let updatedLender = case getPropertyValue prop' of
                                                              Just str -> trace (show prop') lender{lenderZipCode=str}
                                                              Nothing  -> lender
                                        in  trace ("updated lender zip"++lenderZipCode updatedLender) $ LenderView a False b updatedLender d [prop'] f g
                                   else LenderView a True b lender d [prop'] f g)
       ; return $ LenderView inline editing mUser lender props props' itemWebViews editButton
       }
       
lenderIsUser lender Nothing          = False
lenderIsUser lender (Just (login,_)) = lenderLogin lender == login
 
instance Presentable LenderView where
  present (LenderView Full editing mUser lender props props' itemWebViews editButton)   =
        vList [ vSpace 20
              , hList [ (div_ (boxedEx 1 $ image ("leners/" ++ lenderImage lender) ! style "height: 200px")) ! style "width: 204px" ! align "top"
                      , hSpace 20
                      , vList [ h2 $ {- if editing 
                                     then hList [ present fName, nbsp, present lName ] 
                                     else -} (toHtml $ showName lender) -- +++ (with [style "display: none"] $ concatHtml $ map present [fName,lName]) -- todo: not nice!
                              , hList [ vList [ presentProperties $
                                                 (if lenderIsUser lender mUser then getLenderPropsSelf else getLenderPropsEveryone) lender
                                              , vList $ map present props
                                              , vList $ map present props'
                                              , vSpace 20
                                              , present editButton
                                              ]
                                      , hSpace 20
                                      , presentProperties $ getExtraProps lender
                                      ]
                              ]
                      ]
              , vSpace 20
              , h2 $ (toHtml $ "Spullen van "++lenderFirstName lender)
              , vList $ map present itemWebViews
              ]
  present (LenderView Inline editing mUser lender props props' itemWebViews editButton) =
    linkedLender lender $
      hList [ (div_ (boxedEx 1 $ image ("leners/" ++ lenderImage lender) ! style "height: 30px")) ! style "width: 34px" ! align "top"
            , nbsp
            , nbsp
            , vList [ toHtml (showName lender)
                    , vList $ map present props
                    , vList $ map present props'
                    , span_ (presentRating 5 $ lenderRating lender) ! style "font-size: 20px"
                    ]
         --   , with [style "display: none"] $ concatHtml $ map present [fName,lName] ++ [present editButton] -- todo: not nice! 
            ]

getLenderPropsEveryone lender = [ prop | Left prop <- getLenderPropsAll lender ]
getLenderPropsSelf lender  = [ either id id eProp  | eProp <- getLenderPropsAll lender ]

-- Left is both self and others, Right is only self.
getLenderPropsAll lender = [ Right ("LeenClub ID", toHtml $ lenderLogin lender)
                           , Left  ("M/V", toHtml . show $ lenderGender lender)
                           , Left  ("E-mail", toHtml $ lenderMail lender)
                           , Right ("Adres", toHtml $ lenderStreet lender ++ " " ++ lenderStreetNr lender)
                           , Left  ("Postcode", toHtml $ lenderZipCode lender)
                           , Right ("Woonplaats", toHtml $ lenderCity lender)
                           ]

getExtraProps lender = [ ("Rating", with [style "font-size: 20px; position: relative; top: -5px; height: 17px" ] (presentRating 5 $ lenderRating lender)) 
                       , ("Puntenbalans", toHtml . show $ lenderNrOfPoints lender)
                       , ("Aantal spullen", toHtml . show $ length (lenderItems lender))
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

instance Storeable Database ItemsRootView

mkItemsRootView ::WebViewM Database (WebView Database)
mkItemsRootView = mkWebView $
  \vid oldLenderView@(ItemsRootView _ _ _) ->
    do { let namedSortFunctions = [ ("Naam",     compare `on` itemName) 
                                  , ("Prijs",    compare `on` itemPrice)
                                  , ("Eigenaar", compare `on` itemDescr)
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

instance Storeable Database LendersRootView

mkLendersRootView :: WebViewM Database (WebView Database)
mkLendersRootView = mkWebView $
  \vid oldLenderView@(LendersRootView _) ->
    do { let namedSortFunctions = [ ("Voornaam",   compare `on` lenderFirstName) 
                                  , ("Achternaam", compare `on` lenderLastName) 
                                  , ("Rating",     compare `on` lenderRating)
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
data LeenclubPageView = LeenclubPageView User String (EditAction Database) (WebView Database) deriving (Eq, Show, Typeable, Data)

deriveInitial ''LeenclubPageView

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
  TestView Int (Widget RadioView) (Widget (Button Database)) (Widget (TextView Database)) (WebView Database) (WebView Database)
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''TestView

mkTestView :: WebViewM Database (WebView Database)
mkTestView = mkWebView $
  \vid oldTestView@(TestView _ radioOld _ _ _ _) ->
    do { radio <-  mkRadioView ["Naam", "Punten", "Drie"] (getSelection radioOld) True
       ; let radioSel = getSelection radioOld
       ; b <- mkButton "Test button" True $ Edit $ viewEdit vid $ \(TestView a b c d e f) -> TestView 2 b c d e f
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
       ; let wv = TestView radioSel radio b tf wv1' wv2'
       
       --; liftIO $ putStrLn $ "All top-level webnodes "++(show (everythingTopLevel webNodeQ wv :: [WebNode Database])) 
       
       ; return $ wv 
       }

instance Presentable TestView where
  present (TestView radioSel radio button tf wv1 wv2) =
      vList [present radio, present button, present tf, toHtml $ show radioSel, present wv1, present wv2
            ]


instance Storeable Database TestView



data TestView2 = 
  TestView2 (Widget RadioView)  [WebView Database]
           String String 
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''TestView2

mkTestView2 :: WebViewM Database (WebView Database)
mkTestView2 = mkWebView $
  \vid oldTestView@(TestView2 radioOld _ str1 str2) ->
    do { radio <-  mkRadioView ["Edit", "View"] (getSelection radioOld) True
       ; let radioSel = getSelection radioOld
       ; liftIO $ putStr $ show oldTestView
       ; liftIO $ putStrLn $ "radio value " ++ show radioSel
       ; propV1 <- mkPropertyView (radioSel == 0) "Naam" str1 $ \str -> trace ("updating"++str) id
     --  ; propV2 <- mkPropertyView (radioSel == 0) "Straat" str2 $ \str -> viewEdit vid $ \(TestView2 a b c d) -> TestView2 a b  c str
       ; let wv = TestView2 radio [propV1] str1 str2
       
       --; liftIO $ putStrLn $ "All top-level webnodes "++(show (everythingTopLevel webNodeQ wv :: [WebNode Database])) 
       
       ; return $ wv 
       }

instance Presentable TestView2 where
  present (TestView2 radio props p1str p2str) =
      vList [ present radio
            , table $ concatHtml (map present props) 
            , toHtml $ "Property strings: " ++ show p1str ++ " and " ++ show p2str
            ]

instance Storeable Database TestView2


mkTestView3 msg = mkPresentView (\hs -> hList $ toHtml (msg :: String) : hs) $
    do { wv1 <- mkHtmlView $ "een"
       ; wv2 <- mkHtmlTemplateView "test.html" []
       ; return $ [wv1,wv2] 
       }







main :: IO ()
main = server rootViews "LeenclubDB.txt" mkInitialDatabase lenders

rootViews :: RootViews Database
rootViews = [ ("",       mkLeenclubPageView "Home"   mkHomeView), ("test", mkTestView), ("test2", mkTestView2), ("test3", mkTestView3 "msg")
            , ("leners", mkLeenclubPageView "Leners" mkLendersRootView), ("lener", mkLeenclubPageView "Lener" mkLenderRootView)
            , ("items",  mkLeenclubPageView "Spullen" mkItemsRootView),   ("item",  mkLeenclubPageView "Item"  mkItemRootView) 
            , ("geleend", mkLeenclubPageView "Geleend" mkBorrowedRootView)
            , ("login",  mkLeenclubPageView "Login"  mkLeenClubLoginOutView)
            ] 
