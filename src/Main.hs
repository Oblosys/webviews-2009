{-# OPTIONS -fglasgow-exts #-}
module Main where

import Database
import Server
import Types
import Generics -- for testing in ghci

main :: IO ()
main = server                                                                

{-
Check lenses
Check relation to google toolkit. Maybe we can use stuff from it?
-}