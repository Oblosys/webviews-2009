{-# OPTIONS -fglasgow-exts #-}
module Main where

import Database
import Server
import Types (testMonad)


main :: IO ()
main = server                                                                        