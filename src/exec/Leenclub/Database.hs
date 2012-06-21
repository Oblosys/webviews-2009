{-# OPTIONS -XDeriveDataTypeable #-}
module Database where

import Data.Generics
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 

lenders :: Map String (String, String)
lenders = Map.fromList [ ("martijn", ("p", "Martijn"))
                       , ("henny", ("h", "Henny Verweij")) 
                       , ("jaap", ("j", "Jaap Lageman"))
                       ] 
-- TODO: maybe this can be (a special) part of db?

newtype LenderId = LenderId { lenderIdLogin :: String } deriving (Show, Read, Eq, Ord, Typeable, Data)

newtype ItemId = ItemId Int deriving (Show, Read, Eq, Ord, Typeable, Data)


-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allLenders :: Map LenderId Lender, allItems :: Map ItemId Item 
                         }
                  deriving (Eq, Show, Read, Typeable,Data)

data Lender = 
  Lender { lenderId :: LenderId, lenderFirstName :: String, lenderLastName :: String
         , lenderStreet :: String, lenderStreetNr :: String, lenderCity :: String, lenderZipCode :: String
         , lenderCoord :: (Double, Double) -- http://maps.google.com/maps/geo?q=adres&output=xml for lat/long
         , lenderRating :: Int, lenderItems :: [ItemId]
         } deriving (Eq, Show, Read, Typeable, Data)

lenderLogin Lender{lenderId = LenderId login} = login


searchLenders :: String -> Database -> [Lender]
searchLenders term db = [ lender | lender <- Map.elems $ allLenders db
                        , any (isInfixOf term) [lenderFirstName lender, lenderLastName lender, lenderZipCode lender, lenderLogin lender]
                        ]

updateLender :: LenderId -> (Lender -> Lender) -> Database -> Database
updateLender i f db = 
  let lender = unsafeLookup "updateLender" (allLenders db) i
  in  db  { allLenders = Map.insert i (f lender) (allLenders db)
          }
-- add error

removeLender :: LenderId -> Database -> Database
removeLender i db = db { allLenders = Map.delete i (allLenders db) }

{-
newLender :: Database -> (LenLenderatabase)
newLender db =
  let ids = [ i | LenderId i <- map fst (Map.toList $ allLenders db) ]
      newId = LenderId $ if null ids then 0 else (maximum ids + 1)
      newLender = Lender newId "" "" []
  in  ( newLender, db { allLenders = Map.insert newId newLender (allLenders db) } )
  -}

data Category = Book { bookAuthor :: String, bookYear :: Int, bookLanguage :: String, bookGenre :: String, bookPages :: Int, bookISBN :: String}
              | Game { gamePlatform :: String, gameYear :: Int, gamePublisher :: String, gameGenre :: String }
              | CD   { cdArtist :: String, cdYear :: Int, cdGenre :: String }
              | DVD  { dvdNrOfDiscs :: Int, dvdYear :: Int, dvdGenre :: String }
              | Tool {}
              | Electronics {}
              | Misc {} deriving (Eq, Show, Read, Typeable,Data)

data Item = 
  Item { itemId :: ItemId, itemOwner :: LenderId, itemPrice :: Int, itemName :: String, itemDescr :: String
       , itemCategory :: Category
       , itemBorrowed :: Maybe LenderId
       } deriving (Eq, Show, Read, Typeable,Data)

-- put id in element? It is also in the map.

itemIdNr Item{itemId = ItemId i} = i

searchItems :: String -> Database -> [Item]
searchItems term db = [ item | item <- Map.elems $ allItems db
                      , any (isInfixOf term) $ [lenderIdLogin $ itemOwner item, show $ itemPrice item, itemName item, itemDescr item ] ++
                                               (categorySearchFields $ itemCategory item)
                      ]
 where categorySearchFields Book{ bookAuthor=f1, bookGenre=f2} = [f1, f2]
       categorySearchFields Game{ gamePlatform=f1, gamePublisher=f2, gameGenre=f3} = [f1,f2,f3]
       categorySearchFields CD{ cdArtist=f1, cdGenre=f2} = [f1,f2]
       categorySearchFields Tool = []
       categorySearchFields Electronics = []
       categorySearchFields Misc = []
       
updateItem :: ItemId -> (Item -> Item) -> Database -> Database
updateItem i f db = 
  let visit = unsafeLookup "updateItem" (allItems db) i
  in  db  { allItems = Map.insert i (f visit) (allItems db)
          }

removeItem :: ItemId -> Database -> Database
removeItem i db = db { allItems = Map.delete i (allItems db) }

newItem :: LenderId -> Database -> (Item, Database)
newItem uid db =
  let ids = [ i | ItemId i <- map fst (Map.toList $ allItems db) ]
      newId = ItemId $ if null ids then 0 else (maximum ids + 1)
      newItem = Item newId uid 1 "<new>" "" Misc Nothing
  in  ( newItem, db { allItems = Map.insert newId newItem (allItems db) } )

--loginnaam, geslacht, voornaam, tussenvoegsel, achternaam, straatnaam, nr, postcode, lat, long

mkInitialDatabase :: IO (Database)
mkInitialDatabase =
 do { return $ Database 
                (Map.fromList [ (LenderId "martijn", Lender (LenderId "martijn") "Martijn" "Schrage" "Kerkstraat" "15" "Utrecht"
                                                     "3581 RA" (5.1308620,52.0927779)
                                                      4 
                                                      [ItemId 0, ItemId 1, ItemId 2, ItemId 7])
                              , (LenderId "jaap", Lender (LenderId "jaap") "Jaap" "Lageman"  "Kerkstraat" "15" "Utrecht"
                                                     "3581 RA" (5.1309057,52.0927960) 
                                                      5 
                                                      [ItemId 3, ItemId 4])
                              , (LenderId "henny", Lender (LenderId "henny") "Henny" "Verweij"  "Franz Schubertstraat" "39" "Utrecht"
                                                     "3533 GT" (5.0868797,52.0864543) 
                                                      5
                                                      [ItemId 5, ItemId 6])
                ])
                (Map.fromList $ [  (id,item) | item@Item{itemId = id} <- spullen ])
    }

spullen = [ Item (ItemId 0) (LenderId "martijn") 2 "Oblomov"
                                                 "Een boek schrijven van ruim vijfhonderd bladzijden waarin de held bijna tweehonderd bladzijden lang zijn bed niet uit komt, maar dat geen moment verveelt, zoiets is alleen een zeer groot schrijver gegeven. Oblomov, het magnum opus van Ivan Gontsjarov, is een roman die alles mist waar zoveel andere boeken het van moeten hebben; we lezen slechts over de kleine, alledaagse belevenissen van de goedige, maar aartsluie Ilja Oblomov. Zelfs zijn liefde voor de betoverende Olga kan Oblomov niet uit zijn apathie halen en hem aanzetten tot de grootse daden die zij van hem verwacht. De ondergang van een antiheld."
                                                 (Book "Ivan Gontsjarov" 1859 "Engels" "Roman" 552 "9074113052")
                                                 Nothing
          , Item (ItemId 1) (LenderId "martijn") 3 "Grand Theft Auto 4"
                                                 "Wat is er tegenwoordig nog over van die legendarische 'American Dream'? Niko Bellic is net aan wal gestapt na een lange bootreis uit Europa en hoopt in Amerika zijn verleden te begraven. Zijn neef Roman droomt ervan het helemaal te maken in Liberty City, in het land van de onbegrensde mogelijkheden.\nZe raken in de schulden en komen in het criminele circuit terecht door toedoen van oplichters, dieven en ander tuig. Langzamerhand komen ze erachter dat ze hun dromen niet kunnen waarmaken in een stad waar alles draait om geld en status. Heb je genoeg geld, dan staan alle deuren voor je open. Zonder een cent beland je in de goot."
                                                 (Game "Playstation 3" 2011 "Rockstar Games" "Action adventure / open world" )
                                                 Nothing
          , Item (ItemId 2) (LenderId "martijn") 2 "iPhone 3gs"
                                                 "Apple iPhone 3gs, 32Gb"
                                                 Electronics
                                                 (Just $ LenderId "henny")
          , Item (ItemId 3) (LenderId "jaap")    1 "Boormachine"
                                                 "Het krachtige en compacte toestel Krachtig motor van 600 Watt, ideaal om te boren tot 10 mm boordiameter in metaal Bevestiging van boorspil in het lager voor hoge precisie Compact design en gering gewicht voor optimale bediening bij middelzware boortoepassingen Besturings-electronic voor exact aanboren Metalen snelspanboorhouder voor hoge precisie en lange levensduur Rechts- en linksdraaien Bijzonder geschikt voor boorgaten tot 10 mm in staal Functies: Rechts- en linksdraaien Electronic Softgrip Leveromvang: Snelspanboorhouder 1 - 10 mm"
                                                 Tool
                                                 Nothing
          , Item (ItemId 4) (LenderId "jaap")    1 "Spyder calibratie-apparaat"
                                                 "De Datacolor Spyder 4 Elite geeft nauwkeurig en natuurgetrouwe kleuren bij fotobewerkingen, films en games. Daarmee is hij geschikt voor professionele fotografen en andere creatievelingen. Verder is dit de eerste Spyder die iPhone en iPad ready is. Dit betekent dat hij via een app kan kalibreren met deze gadgets en de weergave van kleuren op je smartphone of tablet kan optimaliseren."
                                                 Electronics
                                                 Nothing
          , Item (ItemId 5) (LenderId "henny")   2 "Tomtom"
                                                 "Voor de prijsbewuste bestuurder die toch graag in breedbeeld navigeert is er de TomTom XL Classic. Uitermate gemakkelijk in gebruik - plug de 12V autoadapter in en begin onbezorgd aan je reis. Het IQ Routes systeem zorgt op ieder moment van de dag voor de snelste route."
                                                 Electronics
                                                 (Just $ LenderId "jaap")
          , Item (ItemId 6) (LenderId "henny")   1 "Boormachine"
                                                 "De Makita Accuboormachine BDF343SHE Li-Ion 14,4V beschikt niet alleen over uitzonderlijke krachten, hij is ook nog eens bijzonder comfortabel. Hij heeft namelijk een ergonomisch ontwerp meegekregen van Makita. Zo weet u zeker dat u nooit meer last van uw polsen of ellebogen hebt na het klussen. Daarnaast zorgt het softgrip handvat er voor dat hij erg lekker in de hand ligt. En ook blijft liggen, want zelfs met bezwete handen hanteert u hem nog steeds moeiteloos. Dus wilt u een machine die kracht en gebruikscomfort combineert, dan is de Makita Accuboormachine BDF343SHE Li-Ion 14,4V de juiste keuze."
                                                 Tool
                                                 Nothing
          , Item (ItemId 7) (LenderId "martijn")  1 "Abbey Road"
                                                 ""
                                                 (CD "The Beatles" 1969 "Pop/Rock")
                                                 Nothing
          ]

unsafeLookup tag map key = 
  Map.findWithDefault
    (error $ "Unsafe lookup tagged \""++tag++"\" element "++ show key ++ " not found in " ++ show map) key map
-- do we want extra params such as pig nrs in sub views?
-- error handling database access
-- Unclear: we need both pig ids and subview ids?
-- make clear why we need an explicit view, and the html rep is not enough.



