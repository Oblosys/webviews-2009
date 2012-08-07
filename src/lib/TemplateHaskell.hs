{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell #-}
module TemplateHaskell where

import Language.Haskell.TH
import Types

-- template Haskell code for deriving Initial instances
-- usage: deriveInitial ''DataType     (must appear after the declaration of the data type)
-- Unfortunately, deriveInitial causes declarations to disappear, unless put at the end of the source file (probably a GHC 7.4.1 bug)

deriveInitial :: Name -> Q [Dec]
deriveInitial tName = [d|instance Initial $(conT tName) where
                           initial = $(body) |]
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
       applyN name n = appE (applyN name $ n-1) [e| initial |]
       