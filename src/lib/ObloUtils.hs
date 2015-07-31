{-# LANGUAGE TypeOperators #-}
module ObloUtils where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import GHC.Float (formatRealFloat, FFFormat(FFFixed))
import Data.Label                         -- fclabels
import Control.Category ((.), id)         -- fclabels
import Prelude hiding ((.), id)           -- fclabels
import Control.Arrow (runKleisli, arr)    -- fclabels
import Data.Label.Mono ((:~>), get, modify)   -- fclabels

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of 
                  [(x,"")] -> Just x
                  _        -> Nothing

unsafeRead :: Read a => String -> String -> a
unsafeRead tag str = fromMaybe (error $ "Unsafe read tagged \""++tag++"\" on "++ show str ) $ readMaybe str

unsafeLookup tag key map = 
  Map.findWithDefault
    (error $ "Unsafe lookup tagged \""++tag++"\" element "++ show key ++ " not found in " ++ show map) key map

singleton :: a -> [a]
singleton x = [x]

-- | showFloat shows a floating-point value with a maximum number of decimals and without exponent notation.
showFloat :: Int -> Double -> String
showFloat nrOfDigits f =
    let s = formatRealFloat FFFixed (Just nrOfDigits) f
        s' = dropWhile (== '0') (reverse s)
        s'' = if head s' == '.' then '0':s' else s'
    in reverse s''

showFloats :: Int -> [Double] -> String
showFloats nrOfDigits fs = "[" ++ (concat $ intersperse ", " $ map (showFloat nrOfDigits) fs) ++ "]"

-- | niceFloatFix prints a floating-point value with fixed number of decimals and without exponent notation.
showFloatFix :: Int -> Double -> String
showFloatFix nrOfDigits f =
    let s = formatRealFloat FFFixed (Just nrOfDigits) f
    in  if head s == '.' then '0':s else s


-- fclabels

-- Very hacky adaptation of old fclabels-1.x code to fclabels-2.x.
-- TODO: Can probably be done way more elegantly using new features of fclabels.

mGet :: (f :~> a) -> f -> Maybe a
mGet pl = runKleisli $ Data.Label.Mono.get pl

--mSet :: f :~> a) -> a -> f -> Maybe f
--mSet l v = runKleisli (Data.Label.Mono.set l . arr ((,) v))

mModify :: (f :~> a) -> (a->a) -> f -> Maybe f
mModify l fn = runKleisli (Data.Label.Mono.modify l .  arr ((,) (arr fn)))

unsafeMGet :: String -> (f :~> a) -> f -> a
unsafeMGet tag mLens f = case mGet mLens f of
                           Nothing -> error $ "Partial get failed for " ++ show tag
                           Just a  -> a
                           
unsafeMModify :: String -> (f :~> a) -> (a -> a) -> f -> f                 
unsafeMModify tag mLens fn f = case mModify mLens fn f of
                                 Nothing -> error $ "Partial set failed for " ++ show tag
                                 Just f'  -> f'
                    
--

pLens ::  String -> (f :~> a) -> f :-> a
pLens tag mLens = lens (unsafeMGet tag mLens) (unsafeMModify tag mLens)
