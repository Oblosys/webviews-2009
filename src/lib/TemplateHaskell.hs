{-# OPTIONS -XDeriveDataTypeable -XFlexibleInstances -XTemplateHaskell #-}
module TemplateHaskell where

import Language.Haskell.TH
import Types

{-

The template Haskell code is separate from Types.hs. This causes a bit more verbose code, and postpones some
errors to executable compile time rather than library compile time (mainly undefined-variable errors, as type errors
are not detected at library compile time anyway), but it has as advantage that we don't need to run Types.hs during compilation
(which causes a lot of packages to be imported, making compilation very verbose) 

-}

-- template Haskell code for deriving Initial instances
-- usage: deriveInitial ''DataType     (must appear after the declaration of the data type)
-- Unfortunately, deriveInitial causes declarations to disappear, unless put at the end of the source file (probably a GHC 7.4.1 bug)

deriveInitial :: Name -> Q [Dec]
deriveInitial tName = 
 do { i <- instanceD (return []) (appT (conT $ mkName "Initial") (conT tName)) 
             [ funD (mkName "initial") [ clause [] (normalB body) [] ] ]
    ; return [i]
    }
 where body = do { TyConI (DataD _ _ _ (firstCon:_) _) <- reify tName
                 ; let (cName, cArity) = conNameArity firstCon
                 ; b <- applyN cName cArity
                 ; return b
                 }
       
       conNameArity (NormalC name fields) = (name, length fields)
       conNameArity (RecC name fields) = (name, length fields)  
       conNameArity (InfixC _ name _) = (name, 2)
       conNameArity (ForallC _ _ con) = conNameArity con

       -- e.g. applyN Cons 3 = Cons initial initial initial 
       applyN name 0 = conE name
--       applyN name n = appE (applyN name $ n-1) [e| initial |]
       applyN name n = appE (applyN name $ n-1) (varE $ mkName "initial")
       