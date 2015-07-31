module Main where

import Prelude hiding ((.), id)           -- fclabels
import Control.Category ((.), id)         -- fclabels
import Text.ParserCombinators.Parsec
import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace
import GHC.Exts (fromString)
import Data.Aeson
import Data.Scientific (toRealFloat)
import qualified Data.ByteString.Lazy.Char8 -- only for the IsString instance
import Network.Curl
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Label                         -- fclabels
import ObloUtils

{-
TODO: price in excel, excel in separate files
image in Item
other builders
-}

import DatabaseTypes

builders = [ (buildCD, "cd.csv"), (buildBook, "book.csv"), (buildDVD, "dvd.csv")
           , (buildGame, "game.csv"), (buildTool, "tool.csv") {-, (buildGadget, "gadget.csv"), (buildMisc, "misc.csv") -} ]

main =
 do { lenderStrs <- importCSVFilePP buildLender addLenderCoords "lender.csv"
    ; itemStrs <- fmap concat $ sequence [ importCSVFile build filename | (build, filename) <- builders ]
    ; writeFile "src/exec/BorrowIt/Imported.hs" $ makeModule lenderStrs itemStrs
    --; putStrLn $ concat itemStrs
    }

importCSVFile build filename = importCSVFilePP build (\x -> do { putStrLn ""; return x }) filename

importCSVFilePP :: Show a => (Record -> Maybe a) -> (a -> IO a) -> [Char] -> IO [String]
importCSVFilePP build postProcess filename =
 do { ecsv <- parseCSVFromFile $ "misc/BorrowIt data/" ++ filename
    ; case ecsv of
        Left err  -> error $ show err
        Right csv -> do { let decls = mapMaybe build $ drop 3 csv
                        ; declsPP <- mapM postProcess decls
                        ; return $ map show declsPP
                        }
    }

buildLender :: Record -> Maybe Lender
buildLender fields@[afb,login,voornaam,achternaam,geslacht,email,straat,nr,plaats,postcode,rating, punten] | login /= "" && voornaam /= "" =
  -- trace (show fields) $
  Just $ Lender { _lenderId = (LenderId login)
                , _lenderFirstName = voornaam
                , _lenderLastName = achternaam
                , _lenderGender = if (map toUpper geslacht) == "M" then M else F
                , _lenderMail = email
                , _lenderStreet = straat
                , _lenderStreetNr = nr
                , _lenderCity = plaats
                , _lenderZipCode = postcode
                , _lenderCoords = (0,0) -- http://maps.google.com/maps/geo?q=adres&output=xml for lat/long
                , _lenderImage = afb
                , _lenderRating = fromMaybe 0 $ readMaybe rating
                , _lenderNrOfPoints = fromMaybe 0 $ readMaybe punten
                , _lenderItems = [] -- filled in automatically based on spullen 
                }       
buildLender fields = trace (show $ length fields) Nothing
  
addLenderCoords lender =
 do { let queryString = intercalate " " $ [ get lens lender | lens <- [lenderStreet, lenderStreetNr, lenderCity, lenderZipCode]]
    ; mCoords <- getCoords $ queryString
    ; putStrLn $ "Map query: " ++ queryString ++ " -> " ++ show mCoords
    ; return $ maybe lender (\coords -> lender{ _lenderCoords = coords}) mCoords
    }

getCoords :: String -> IO (Maybe (Double,Double))
getCoords address =
 do { let escapedAddress = concatMap (\c -> if c==' ' then "%20" else [c]) address
    ; (_,text) <- curlGetString ("http://maps.google.com/maps/geo?q="++escapedAddress) []
    ; let Just (Object value) = decode (fromString text) 
    ; let coords = do { (Array objArr) <- HM.lookup (fromString "Placemark") value -- Maybe monad
                      ; addressDetails <- case V.toList objArr of
                          Object addressDetails:_ -> Just addressDetails
                          _                       -> Nothing
                      ; (Object coords) <- HM.lookup (fromString "Point") addressDetails 
                      ;  (Array latlong) <- HM.lookup (fromString "coordinates") coords
                      ; case V.toList latlong of
                          Number scLat : Number scLong :_ -> Just (toRealFloat scLat, toRealFloat scLong)
                          _                                 -> Nothing
                      }      
    ; return coords
    }


buildCD :: Record -> Maybe Item
buildCD fields@[afb,eigenaar,titel,artiest,uitvoerende,jaartal,genre,staat, punten,beschrijving] | eigenaar /= "" && titel /= "" =
  -- trace (show fields) $
  Just $ Item { _itemId = ItemId $ -1
              , _itemOwner = LenderId eigenaar
              , _itemPrice = fromMaybe 0 $ readMaybe punten
              , _itemName = titel
              , _itemDescr = beschrijving
              , _itemState = staat
              , _itemImage = afb 
              , _itemBorrowed = Nothing
              , _itemCategory = CD artiest (fromMaybe 0 $ readMaybe jaartal) genre
              }
buildCD fields = trace ("buildCD: " ++ show (length fields)++"\n"++show fields) Nothing

 
buildBook :: Record -> Maybe Item
buildBook fields@[afb,eigenaar,titel,auteur,taal,jaartal,genre,aantalPag,isbn,staat, punten,beschrijving] | eigenaar /= "" && titel /= "" =
  -- trace (show fields) $
  Just $ Item { _itemId = ItemId $ -1
              , _itemOwner = LenderId eigenaar
              , _itemPrice = fromMaybe 0 $ readMaybe punten
              , _itemName = titel
              , _itemDescr = beschrijving
              , _itemState = staat
              , _itemImage = afb 
              , _itemBorrowed = Nothing
              , _itemCategory = Book { _bookAuthor = auteur
                                     , _bookYear = fromMaybe 0 $ readMaybe jaartal
                                     , _bookLanguage = taal
                                     , _bookGenre = genre
                                     , _bookPages = fromMaybe 0 $ readMaybe aantalPag
                                     , _bookISBN = isbn
                                     }
              }
buildBook fields = trace ("buildBook: " ++ show (length fields)++"\n"++show fields) Nothing


buildDVD :: Record -> Maybe Item
buildDVD fields@[afb,eigenaar,filmSerie,titel,regisseur,taal,jaartal,genre,speelduur,imdb,seizoen,aantalAfl,staat, punten,beschrijving] | eigenaar /= "" && titel /= "" =
  -- trace (show fields) $
  Just $ Item { _itemId = ItemId $ -1
              , _itemOwner = LenderId eigenaar
              , _itemPrice = fromMaybe 0 $ readMaybe punten
              , _itemName = titel
              , _itemDescr = beschrijving
              , _itemState = staat
              , _itemImage = afb 
              , _itemBorrowed = Nothing
              , _itemCategory = DVD { _dvdMovieOrSeries =  if (map toUpper filmSerie) == "film" then Movie else Series
                                    , _dvdDirector = regisseur
                                    , _dvdLanguage = taal
                                    , _dvdYear = fromMaybe 0 $ readMaybe jaartal
                                    , _dvdGenre = genre
                                    , _dvdRunningTime = fromMaybe 0 $ readMaybe speelduur
                                    , _dvdIMDb = imdb
                                    , _dvdSeason = fromMaybe 0 $ readMaybe seizoen
                                    , _dvdNrOfEpisodes = fromMaybe 0 $ readMaybe aantalAfl
                                    }
                     
              }
buildDVD fields = trace ("buildDVD: " ++ show (length fields)++"\n"++show fields) Nothing

buildGame :: Record -> Maybe Item
buildGame fields@[afb,eigenaar,titel,platform,jaartal,genre,developer,staat, punten,beschrijving] | eigenaar /= "" && titel /= "" =
  -- trace (show fields) $
  Just $ Item { _itemId = ItemId $ -1
              , _itemOwner = LenderId eigenaar
              , _itemPrice = fromMaybe 0 $ readMaybe punten
              , _itemName = titel
              , _itemDescr = beschrijving
              , _itemState = staat
              , _itemImage = afb 
              , _itemBorrowed = Nothing
              , _itemCategory = Game { _gamePlatform = platform
                                     , _gameYear = fromMaybe 0 $ readMaybe jaartal
                                     , _gameDeveloper = developer
                                     , _gameGenre = genre
                                     }
                     
              }
buildGame fields = trace ("buildGame: " ++ show (length fields)++"\n"++show fields) Nothing

buildTool :: Record -> Maybe Item
buildTool fields@[afb,eigenaar,soort,merk,typenr,bouwjaar,staat, punten,beschrijving] | eigenaar /= "" && soort /= "" =
  -- trace (show fields) $
  Just $ Item { _itemId = ItemId $ -1
              , _itemOwner = LenderId eigenaar
              , _itemPrice = fromMaybe 0 $ readMaybe punten
              , _itemName  = soort
              , _itemDescr = beschrijving
              , _itemState = staat
              , _itemImage = afb 
              , _itemBorrowed = Nothing
              , _itemCategory = Tool { _toolBrand = merk
                                     , _toolType = typenr
                                     , _toolYear = fromMaybe 0 $ readMaybe bouwjaar
                                     }
                     
              }
buildTool fields = trace ("buildTool: " ++ show (length fields)++"\n"++show fields) Nothing

-- Gadget and Misc have no fixed data type yet

 {-
 data Lender = 
  Lender { lenderId :: LenderId, lenderFirstName :: String, lenderLastName :: String, lenderGender :: Gender 
         , lenderMail :: String
         , lenderStreet :: String, lenderStreetNr :: String, lenderCity :: String, lenderZipCode :: String
         , lenderCoord :: (Double, Double) -- http://maps.google.com/maps/geo?q=adres&output=xml for lat/long
         , lenderImage :: String
         , lenderRating :: Int, lenderItems :: [ItemId]
         } deriving (Eq, Show, Read, Typeable, Data)
         
data Category = Book { bookAuthor :: String, bookYear :: Int, bookLanguage :: String, bookGenre :: String, bookPages :: Int, bookISBN :: String}
              | Game { gamePlatform :: String, gameYear :: Int, gameDeveloper :: String, gameGenre :: String }
              | CD   { cdArtist :: String, cdYear :: Int, cdGenre :: String }
              | DVD  { dvdMovieOrSeries :: MovieOrSeries, dvdDirector :: String, dvdLanguage :: String, dvdYear :: Int, dvdGenre :: String
                     , dvdRunningTime :: Int, dvdIMDb :: String, dvdSeason :: Int, dvdNrOfEpisodes :: Int }
              | Tool { toolBrand :: String, toolType :: String, toolYear :: Int }
              | Electronics {}
              | Misc {} deriving (Eq, Show, Read, Typeable,Data)

data Item = 
  Item { itemId :: ItemId, itemOwner :: LenderId, itemPrice :: Int, itemName :: String, itemDescr :: String, itemState :: String
       , itemImage :: String
       , itemCategory :: Category
       , itemBorrowed :: Maybe LenderId
       } deriving (Eq, Show, Read, Typeable,Data)

 -}
 
makeModule lenderDecls itemDecls = "module Imported where\n\nimport DatabaseTypes\n\nlenders = [ " ++
                       intercalate "\n         , " lenderDecls ++ "\n         ]\nitems = [ " ++
                       intercalate "\n        , " itemDecls ++ "\n        ]"
 
type CSV = [Record]

-- | A record is a series of fields
type Record = [Field]

-- | A field is a string
type Field = String

-- | A Parsec parser for parsing CSV files
csv :: Parser CSV
csv = do x <- record `sepEndBy` many1 (oneOf "\n\r")
         eof
         return x

record :: Parser Record
record = do { field <- (quotedField <|> field)
            ; return $ interpretNewlines field 
            }  `sepBy` char ';'

interpretNewlines "" = ""
interpretNewlines ('\\':'n':rest) = '\n' : interpretNewlines rest
interpretNewlines (c:rest)       = c : interpretNewlines rest

field :: Parser Field
field = many (noneOf ";\n\r\"")

quotedField :: Parser Field
quotedField = between (char '"') (char '"') $
              many (noneOf "\"" <|> try (string "\"\"" >> return '"'))

-- | Given a file name (used only for error messages) and a string to
-- parse, run the parser.
parseCSV :: FilePath -> String -> Either ParseError CSV
parseCSV = parse csv

-- | Given a file name, read from that file and run the parser
parseCSVFromFile :: FilePath -> IO (Either ParseError CSV)
parseCSVFromFile csvFile = --parseFromFile doesn't handle UTF8 well
 do { contents <- readFile csvFile
    ; return $ parseCSV "cd" contents
    }