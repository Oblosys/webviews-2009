
module Database where

type Database = [(String, String)]

mkInitialDatabase :: IO Database
mkInitialDatabase = return []