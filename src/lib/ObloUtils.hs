module ObloUtils where

import Data.Maybe
import qualified Data.Map as Map

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
