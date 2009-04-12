{-# OPTIONS -fglasgow-exts #-}
module Types where

import Data.Generics
import Database

import Text.Show.Functions

newtype Id = Id Int deriving (Show, Eq, Ord, Data, Typeable)
noId = Id (-1)

data EString = EString { getStrId :: Id, getStrVal :: String } deriving (Show, Typeable, Data)

estr str = EString noId str

data EInt = EInt { getIntId :: Id, getIntVal :: Int } deriving (Show, Typeable, Data)

eint i = EInt noId i

data Button = Button { getButtonId :: Id, getCommand :: Database -> Database } deriving (Show, Typeable, Data)


--instance Show (a->a) where
--  show f = "<function>"
 
-- is text part of Button or the presentation?
-- is the action part of the button?
-- error handling! why don't internal errors show up on stderr?
-- id handling in views is buggy now