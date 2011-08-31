module Main where

import Database
import Server
import Types
import Generics -- for testing in ghci
import System.IO

main :: IO ()
main = do { hSetBuffering stdout NoBuffering -- necessary to run server in Eclipse
          ; server 
          }    
                                                                  

{-
Check lenses
Check relation to google toolkit. Maybe we can use stuff from it?
-}