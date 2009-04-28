{-# OPTIONS -fglasgow-exts #-}
module Main where

import Database
import Server
import Types (testMonad)
import Generics -- for testing in ghci

main :: IO ()
main = server                                                                        