module Database ( module DatabaseTypes
                , mkInitialDatabase
                , users
                , updateLender
                , searchLenders
                , getOwnedItems
                , getLendedItems
                , getBorrowedItems
                , updateItem 
                , searchItems
                , deleteItem
                , insertAsNewItem
                , getItemCategoryName
                , emptyItem, emptyBook, emptyGame, emptyCD, emptyDVD, emptyTool
                ) where

import Data.Generics
import Data.List
import Data.Maybe
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import ObloUtils
import DatabaseTypes
import qualified Imported
import Control.Category hiding (Category) -- fclabels
import Data.Label                         -- fclabels
import Prelude hiding ((.), id)           -- fclabels
import Debug.Trace



users :: Map String (String, String)
users = Map.fromList [ ("martijn", ("p", "Martijn"))
                     , ("Henny", ("h", "Henny Verweij")) 
                     , ("Jaap", ("j", "Jaap Lageman"))
                     ] 
-- TODO: maybe this can be (a special) part of db?



searchLenders :: String -> Database -> [Lender]
searchLenders term db = [ lender | lender <- Map.elems $ get allLenders db
                        , any (\str -> map toLower term `isInfixOf` map toLower str) 
                              [get lenderFirstName lender, get lenderLastName lender, get lenderZipCode lender, get (lenderIdLogin . lenderId) lender]
                        ]

updateLender :: LenderId -> (Lender -> Lender) -> Database -> Database
updateLender i f db = 
  let lender = unsafeLookup "updateLender" i $ get allLenders db
  in  modify allLenders (Map.insert i (let r = f lender in trace (show r) r)) db

-- add error

removeLender :: LenderId -> Database -> Database
removeLender i = modify allLenders (Map.delete i)

{-
newLender :: Database -> (LenLenderatabase)
newLender db =
  let ids = [ i | LenderId i <- map fst (Map.toList $ allLenders db) ]
      newId = LenderId $ if null ids then 0 else (maximum ids + 1)
      newLender = Lender newId "" "" []
  in  ( newLender, db { allLenders = Map.insert newId newLender (allLenders db) } )
  -}

getItemIdNr item = get (itemIdNr . itemId) item

noItemId :: ItemId
noItemId = ItemId (-1)

assignUniqueItemIds :: [Item] -> [Item]
assignUniqueItemIds items = [ set itemId (ItemId i) item | (i,item) <- zip [0..] items ] 

-- assign 
assignItems :: [Item] -> [Lender] -> [Lender]
assignItems allItems allLenders = [ set lenderItems myItems lender 
                                  | lender <- allLenders 
                                  , let myItems = map (get itemId) $ filter ((==get lenderId lender) .get itemOwner) allItems
                                  ] 
 
searchItems :: String -> Database -> [Item]
searchItems term db = [ item | item <- Map.elems $ get allItems db
                      , any (\str -> map toLower term `isInfixOf` map toLower str) $ 
                            [get (lenderIdLogin . itemOwner) item, show $ get itemPrice item, get itemName item, get itemDescr item ] ++
                            (categorySearchFields $ get itemCategory item)
                      ]
 where categorySearchFields Book{ _bookAuthor=f1, _bookGenre=f2} = [f1, f2]
       categorySearchFields Game{ _gamePlatform=f1, _gameDeveloper=f2, _gameGenre=f3} = [f1,f2,f3]
       categorySearchFields DVD{ _dvdMovieOrSeries=f1, _dvdDirector=f2, _dvdGenre=f3 } = [if f1 == Movie then "film" else "serie",f2,f3]
       categorySearchFields CD{ _cdArtist=f1, _cdGenre=f2 } = [f1,f2]
       categorySearchFields Tool{} = []
       categorySearchFields Electronics = []
       categorySearchFields Misc = []

-- this is more efficient than looking up the lender and looking up all of its owned itemId's
getOwnedItems :: LenderId -> Database -> [Item]
getOwnedItems ownerId db = filter ((ownerId ==) . (get itemOwner)) $ Map.elems (get allItems db) 

getLendedItems :: LenderId -> Database -> [Item]
getLendedItems ownerId db =  filter (isJust . get itemBorrowed) $ getOwnedItems ownerId db 

getBorrowedItems :: LenderId -> Database -> [Item]
getBorrowedItems lenderId db = filter (maybe False (\borrower -> borrower == lenderId) . get itemBorrowed) $ Map.elems (get allItems db)

updateItem :: ItemId -> (Item -> Item) -> Database -> Database
updateItem i f db = 
  let visit = unsafeLookup "updateItem" i $ get allItems db
  in  modify allItems (Map.insert i (f visit)) db


-- Remove the item from allItems as well as from the lenderItems field of its owner.
deleteItem :: Item -> Database -> Database
deleteItem item db =
  let db' = updateLender (get itemOwner item) (removeLenderItem (get itemId item)) db
  in  modify allItems (Map.delete (get itemId item)) db' 
 where removeLenderItem :: ItemId -> Lender -> Lender
       removeLenderItem iId = modify lenderItems $ \is -> 
                                                     let is' = delete iId is
                                                     in  trace (show is ++ "\n"++ show (get itemId item)++"\n"++show is') $
                                                         is'
                                                                       
-- Assign a fresh id to newItem and insert in database as well as lenderItems field of its owner.
insertAsNewItem :: Item -> Database -> Database
insertAsNewItem itemWithoutId db =
  let ids = [ i | ItemId i <- map fst (Map.toList $ get allItems db) ]
      newId = ItemId $ if null ids then 0 else (maximum ids + 1)
      newItem = set itemId newId itemWithoutId
      db' = modify allItems (Map.insert newId newItem) db
  in  updateLender (get itemOwner newItem) (modify lenderItems $ (newId :) ) db' 

getItemCategoryName :: Item -> String
getItemCategoryName item = case get itemCategory item of 
                             Book{}        -> "Boek"
                             Game{}        -> "Game"
                             CD{}          -> "CD"
                             DVD{}         -> "DVD"
                             Tool{}        -> "Gereedschap"
                             Electronics{} -> "Gadget"
                             Misc{}        -> "Misc"

emptyItem :: LenderId -> Category -> Item
emptyItem lenderId category = Item (ItemId $ -1) lenderId 0 "" "" "" genericImage category Nothing
  where genericImage = case category of
                         Book{} -> "book.png"
                         Game{} -> "game.png"
                         CD{} -> "cd.png"
                         DVD{} -> "dvd.png"
                         Tool{} -> "tool.png"
                         
emptyBook, emptyGame, emptyCD, emptyDVD, emptyTool :: LenderId -> Item
emptyBook lenderId = emptyItem lenderId $ Book "" 0 "" "" 0 ""
emptyGame lenderId = emptyItem lenderId $ Game "" 0 "" ""
emptyCD   lenderId = emptyItem lenderId $ CD "" 0 ""
emptyDVD lenderId = emptyItem lenderId $ DVD Movie "" "" 0 "" 0 "" 0 0
emptyTool lenderId = emptyItem lenderId $ Tool "" "" 0

mkInitialDatabase :: IO Database
mkInitialDatabase = return initialDatabase
 
initialDatabase :: Database
initialDatabase = Database 
                   (Map.fromList $ map (\l -> (get lenderId l, l)) $ assignItems spullen Imported.lenders)
                   (Map.fromList [  (get itemId item,item) | item <- spullen ])

spullen = assignUniqueItemIds $
          [ Item noItemId (LenderId "martijn") 3 "Grand Theft Auto 4"
                                                 "Wat is er tegenwoordig nog over van die legendarische 'American Dream'? Niko Bellic is net aan wal gestapt na een lange bootreis uit Europa en hoopt in Amerika zijn verleden te begraven. Zijn neef Roman droomt ervan het helemaal te maken in Liberty City, in het land van de onbegrensde mogelijkheden.\nZe raken in de schulden en komen in het criminele circuit terecht door toedoen van oplichters, dieven en ander tuig. Langzamerhand komen ze erachter dat ze hun dromen niet kunnen waarmaken in een stad waar alles draait om geld en status. Heb je genoeg geld, dan staan alle deuren voor je open. Zonder een cent beland je in de goot."
                                                 "Uitstekend"
                                                 "1.jpg"
                                                 (Game "Playstation 3" 2011 "Rockstar Games" "Action adventure / open world" )
                                                 (Just $ LenderId "Jaap")
          , Item noItemId (LenderId "martijn") 2 "iPhone 3gs"
                                                 "Apple iPhone 3gs, 32Gb"
                                                 "Paar krasjes"
                                                 "2.jpg"
                                                 Electronics
                                                 (Just $ LenderId "Henny")
          , Item noItemId (LenderId "Jaap")    1 "Boormachine"
                                                 "Het krachtige en compacte toestel Krachtig motor van 600 Watt, ideaal om te boren tot 10 mm boordiameter in metaal Bevestiging van boorspil in het lager voor hoge precisie Compact design en gering gewicht voor optimale bediening bij middelzware boortoepassingen Besturings-electronic voor exact aanboren Metalen snelspanboorhouder voor hoge precisie en lange levensduur Rechts- en linksdraaien Bijzonder geschikt voor boorgaten tot 10 mm in staal Functies: Rechts- en linksdraaien Electronic Softgrip Leveromvang: Snelspanboorhouder 1 - 10 mm"
                                                 "Goed"
                                                 "3.jpg"
                                                 (Tool "Bosch" "XP33" 2005)
                                                 Nothing
          , Item noItemId (LenderId "Jaap")    1 "Spyder calibratie-apparaat"
                                                 "De Datacolor Spyder 4 Elite geeft nauwkeurig en natuurgetrouwe kleuren bij fotobewerkingen, films en games. Daarmee is hij geschikt voor professionele fotografen en andere creatievelingen. Verder is dit de eerste Spyder die iPhone en iPad ready is. Dit betekent dat hij via een app kan kalibreren met deze gadgets en de weergave van kleuren op je smartphone of tablet kan optimaliseren."
                                                 "Goed"
                                                 "4.jpg"
                                                 Electronics
                                                 Nothing
          , Item noItemId (LenderId "Henny")   2 "Tomtom"
                                                 "Voor de prijsbewuste bestuurder die toch graag in breedbeeld navigeert is er de TomTom XL Classic. Uitermate gemakkelijk in gebruik - plug de 12V autoadapter in en begin onbezorgd aan je reis. Het IQ Routes systeem zorgt op ieder moment van de dag voor de snelste route."
                                                 "Goed"
                                                 "5.jpg"
                                                 Electronics
                                                 (Just $ LenderId "Jaap")
          , Item noItemId (LenderId "Henny")   1 "Boormachine"
                                                 "De Makita Accuboormachine BDF343SHE Li-Ion 14,4V beschikt niet alleen over uitzonderlijke krachten, hij is ook nog eens bijzonder comfortabel. Hij heeft namelijk een ergonomisch ontwerp meegekregen van Makita. Zo weet u zeker dat u nooit meer last van uw polsen of ellebogen hebt na het klussen. Daarnaast zorgt het softgrip handvat er voor dat hij erg lekker in de hand ligt. En ook blijft liggen, want zelfs met bezwete handen hanteert u hem nog steeds moeiteloos. Dus wilt u een machine die kracht en gebruikscomfort combineert, dan is de Makita Accuboormachine BDF343SHE Li-Ion 14,4V de juiste keuze."
                                                 "Goed"
                                                 "6.jpg"
                                                 (Tool "Makita" "BDF343SHE" 2009)
                                                 Nothing
          
          ] ++ Imported.items

-- do we want extra params such as pig nrs in sub views?
-- error handling database access
-- Unclear: we need both pig ids and subview ids?
-- make clear why we need an explicit view, and the html rep is not enough.



