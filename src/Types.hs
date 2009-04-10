module Types where

import Data.Generics

newtype Id = Id Int deriving (Show, Eq, Ord, Data, Typeable)
noId = Id (-1)

data EString = EString {getStrId :: Id, getStrVal :: String} deriving (Show, Typeable, Data)

estr str = EString noId str

data EInt = EInt {getIntId :: Id, getIntVal :: Int} deriving (Show, Typeable, Data)

eint i = EInt noId i
