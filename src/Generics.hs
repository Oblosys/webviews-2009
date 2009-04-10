module Generics where

import Types

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 

-- Generics


-- number all Id's in the argument data structure uniquely
assignIds :: Data d => d -> d
assignIds x = snd $ everywhereAccum assignId 0 x

assignId :: Data d => Int -> d -> (Int,d)
assignId = mkAccT $ \i (Id _) -> (i+1, Id i)


type Updates = Map Id String  -- maps id's to the string representation of the new value

-- update the datastructure at the id's in Updates 
replace :: Data d => Updates -> d -> d
replace updates = everywhere $ extT (mkT (replaceEString updates))  (replaceEInt updates)

replaceEString :: Updates -> EString -> EString
replaceEString updates x@(EString i _) =
  case Map.lookup i updates of
    Just str -> (EString i str)
    Nothing -> x

replaceEInt :: Updates -> EInt -> EInt
replaceEInt updates x@(EInt i _) =
  case Map.lookup i updates of
    Just str -> (EInt i (read str))
    Nothing -> x



everywhereAccum :: Data d => (forall b . Data b => a -> b -> (a,b)) -> a -> d -> (a,d)
everywhereAccum f acc a =
 let (acc'',r) = gfoldl k z a
 in  f acc'' r  
 where k (acc',a2b) a = let (acc'',a') = everywhereAccum f acc' a
                            b = a2b a'
                        in  (acc'', b)
       z e = (acc, e)

gReplace :: forall a d . (Typeable a, Data d) => [a] -> d -> ([a],d)
gReplace = mkAccT $ \(i:is) _ -> (is,i)

number :: Data d => Int -> d -> (Int,d)
number  = mkAccT $ \acc _ -> (acc+1, acc)

mkAccT :: forall a b acc . (Typeable acc, Typeable a, Data b) => (acc -> a -> (acc,a)) -> (acc -> b -> (acc,b))
mkAccT f = case cast f  of -- can we do this without requiring Typeable acc?
                  Just g  -> g 
                  Nothing -> \acc b -> (acc,b)

