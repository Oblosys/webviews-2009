{-# OPTIONS -fglasgow-exts #-}
module Generics where

import Types

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 

-- Generics


data Tree = Bin Tree Tree | Leaf Char deriving (Show, Data, Typeable)

mytree = Bin (Bin (Leaf 'a') (Leaf 'b')) (Leaf 'c')

mkViewMap :: WebView -> ViewMap
mkViewMap wv = let wvs = getAll wv
                    in  Map.fromList [ (i, wv) | wv@(WebView i _ _) <- wvs]

-- lookup the view id and if the associated view is of the desired type, return it. Otherwise
-- return initial
getOldView :: (Initial v, Typeable v) => ViewId -> ViewMap -> v
getOldView vid viewMap = 
  case Map.lookup vid viewMap of
    Nothing              -> initial
    Just (WebView _ _ aView) -> case cast aView of
                                  Nothing      -> initial
                                  Just oldView -> oldView

getAll :: (Data a, Data b) => a -> [b]
getAll = listify (const True)

getTopLevelWebViews :: Data a => a -> [WebView]
getTopLevelWebViews wv = everythingBut (Nothing `mkQ` Just) wv
-- the type sig specifies the term type

-- is everythingBut, but not on the root (not used now)
everythingBelow :: Data r => GenericQ (Maybe r) -> GenericQ [r]
everythingBelow f x = foldl (++) [] (gmapQ (everythingBut f) x)

-- get all non-nested descendents of a type
-- Eg. everythingBelow () (X (T_1 .. (T_2 .. (T_3 ..) ..)) (T_4)) = [T_1,T_2,T_4]
everythingBut :: Data r => GenericQ (Maybe r) -> GenericQ [r]
everythingBut f x = case f x of
                      Just x  -> [x]
                      Nothing -> foldl (++) [] (gmapQ (everythingBut f) x)

getWebViewContainingId :: Id -> WebView -> Maybe WebView
getWebViewContainingId (Id i) wv = 
  case somethingAcc Nothing (Nothing `mkQ` Just) (Nothing `mkQ` isId) wv of
    Just (_, Just wv') -> Just wv'
    Just (_, Nothing)  -> Nothing -- not called on a webview
    Nothing -> Nothing -- id not found
  where isId (Id i') = if i == i' then Just i' else Nothing

somethingAcc :: (Data a, Data r) => a -> GenericQ (Maybe a) -> GenericQ (Maybe r) -> GenericQ (Maybe (r,a))
somethingAcc a fa f x = let a' = case fa x of 
                                   Nothing -> a
                                   Just a' -> a'
                        in  case f x of
                              Just x  -> Just (x,a')
                              Nothing -> foldl orElse Nothing (gmapQ (somethingAcc a' fa f) x)

{-
everythingBut :: Data a 
 => GenericQ Bool
 -> (forall a. Data a => a -> r) 
 -> a -> r 
everythingBut q f x = {- if q x then [x] else -} foldl (++) [] (gmapQ (everythingBut q f) x)
-}

-- number all Id's in the argument data structure uniquely
assignIds :: Data d => d -> d
assignIds x = snd $ everywhereAccum assignId 0 x

assignId :: Data d => Int -> d -> (Int,d)
assignId = mkAccT $ \i (Id _) -> (i+1, Id i)


type Updates = Map Id String  -- maps id's to the string representation of the new value

-- update the datastructure at the id's in Updates 
replace :: Data d => Updates -> d -> d
replace updates v = (everywhere $ extT (mkT (replaceEString updates))  (replaceEInt updates)) v

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

replaceWebViewById :: (ViewId) -> WebView -> WebView -> WebView
replaceWebViewById i wv rootView =
 (everywhere $ mkT replaceWebView) rootView
 where replaceWebView wv'@(WebView i' _ _) = if i == i' then wv else wv' 
--getWebViews x = listify (\(WebView i' :: WebView) -> True) x 

getButtonById :: Data d => Id -> d -> Button
getButtonById i view = 
  case listify (\(Button i' _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: no button with id "++show i
    _   -> error $ "internal error: multiple buttons with id "++show i

--

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


getWebViewById i view = 
  case listify (\(WebView i' _ _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: no button with id "
    _   -> error $ "internal error: multiple buttons with id "

