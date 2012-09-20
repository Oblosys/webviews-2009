{-# LANGUAGE TypeOperators #-}
module ObloUtils where

import Data.Maybe
import qualified Data.Map as Map
import Control.Category hiding (Category) -- fclabels
import Data.Label                         -- fclabels
import Prelude hiding ((.), id)           -- fclabels
import qualified Data.Label.Maybe         -- fclabels

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of 
                  [(x,"")] -> Just x
                  _        -> Nothing

unsafeRead :: Read a => String -> String -> a
unsafeRead tag str = fromMaybe (error $ "Unsafe read tagged \""++tag++"\" on "++ show str ) $ readMaybe str

unsafeLookup tag map key = 
  Map.findWithDefault
    (error $ "Unsafe lookup tagged \""++tag++"\" element "++ show key ++ " not found in " ++ show map) key map

singleton :: a -> [a]
singleton x = [x]



-- fclabels

mGet :: (f Data.Label.Maybe.:~> a) -> f -> Maybe a
mGet = Data.Label.Maybe.get

mSet :: (f Data.Label.Maybe.:~> a) -> a -> f -> Maybe f
mSet = Data.Label.Maybe.set

unsafeMGet :: String -> (f Data.Label.Maybe.:~> a) -> f -> a
unsafeMGet tag mLens f = case mGet mLens f of
                           Nothing -> error $ "Partial get failed for " ++ show tag
                           Just a  -> a
                           
unsafeMSet :: String -> (f Data.Label.Maybe.:~> a) -> a -> f -> f                 
unsafeMSet tag mLens a f = case mSet mLens a f of
                             Nothing -> error $ "Partial set failed for " ++ show tag
                             Just f  -> f
                    
pLens ::  String -> (f Data.Label.Maybe.:~> a) -> f :-> a
pLens tag mLens = lens (unsafeMGet tag mLens) (unsafeMSet tag mLens)
