module Main where

import Prelude hiding (readFile)
import Text.ParserCombinators.Parsec
import System.IO.UTF8 (readFile)
import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace

{-
TODO: price in excel, excel in separate files
image in Item
other builders
-}

import Database
builders = [ (buildCD, "cd.csv") {-, (buildBook, "book.csv"), (buildDvd, "dvd.csv")
           , (buildGame, "game.csv"), (buildTool, "tool.csv"), (buildGadget, "gadget.csv"), (buildMisc, "misc.csv") -} ]

main =
 do { lenderStrs <- importCSVFile buildLender "lender.csv"
    ; itemStrs <- fmap concat $ sequence [ importCSVFile build filename | (build, filename) <- builders ]
    ; writeFile "src/exec/Leenclub/Imported.hs" $ makeModule lenderStrs itemStrs
    ; putStrLn $ concat itemStrs
    }
    
importCSVFile build filename =
 do { ecsv <- parseCSVFromFile $ "LeenclubImport/" ++ filename
    ; case ecsv of
        Left err  -> error $ show err
        Right csv -> return $ map show $ mapMaybe build $ drop 3 csv
    }

buildLender :: Record -> Maybe Lender
buildLender fields@[afb,login,voornaam,achternaam,geslacht,email,straat,nr,plaats,postcode,rating] | login /= "" && voornaam /= "" =
  -- trace (show fields) $
  Just $ Lender { lenderId = (LenderId login)
                , lenderFirstName = voornaam
                , lenderLastName = achternaam
                , lenderGender = if (map toUpper geslacht) == "M" then M else F
                , lenderMail = email
                , lenderStreet = straat
                , lenderStreetNr = nr
                , lenderCity = plaats
                , lenderZipCode = postcode
                , lenderCoord = (0,0) -- http://maps.google.com/maps/geo?q=adres&output=xml for lat/long
                , lenderImage = afb
                , lenderRating = fromMaybe 0 $ readMaybe rating
                , lenderItems = [] -- filled in automatically based on spullen 
                }       
buildLender fields = trace (show $ length fields) Nothing
  
buildCD :: Record -> Maybe Item
buildCD fields@[afb,eigenaar,naam,artiest,uitvoerende,jaartal,genre,staat, punten,beschrijving] | eigenaar /= "" && naam /= "" =
  -- trace (show fields) $
  Just $ Item (ItemId $ -1) (LenderId eigenaar) (fromMaybe 0 $ readMaybe punten) 
              naam beschrijving staat afb (CD artiest (fromMaybe 0 $ readMaybe jaartal) genre) Nothing
buildCD fields = trace (show $ length fields) Nothing

 
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







readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of 
                  [(x,"")] -> Just x
                  _        -> Nothing
             
