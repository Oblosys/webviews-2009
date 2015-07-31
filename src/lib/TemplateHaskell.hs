{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TemplateHaskell #-}
module TemplateHaskell where

import Language.Haskell.TH
import Types
import Control.Applicative

-- template Haskell code for deriving Initial instances
-- usage: deriveInitial ''DataType     (must appear after the declaration of the data type)
-- Unfortunately, deriveInitial causes declarations to disappear, unless put at the end of the source file (probably a GHC 7.4.1 bug)

deriveInitial :: Name -> Q [Dec]
deriveInitial tName = [d|instance Initial $(conT tName) where
                           initial = $(body) |]
 where body = do { (firstCon:_) <- getConstructors "deriveInitial" tName
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

{-
different MapWebView instances:

instance MapWebView db Type           with Type referring to Database

instance MapWebView Database Type     with Type referring to Database

instance MapWebView db (Type db)

TODO: check for recursive appearances of Widget or WebView and use pure if there are none.
TODO: use introspection for determining which deriveWebView to use? (check if a type appears as arg to WebView or Widget (w _))
TODO: generate a deriveMapWebView function that already includes the database
      this one can also generate nice type synonyms for WebView Database and Widgets

TODO: make lenses for WebViews
 
-}       
deriveMapWebView :: Name -> Q [Dec]
deriveMapWebView tName = deriveMapWebViewEx True Nothing tName

deriveMapWebViewDb :: Name -> Name -> Q [Dec]
deriveMapWebViewDb dbName tName = deriveMapWebViewEx False (Just dbName) tName


deriveMapWebViewEx :: Bool -> Maybe Name -> Name -> Q [Dec]
deriveMapWebViewEx parameterizedWithDb mDbName tpName = 
  [d|instance MapWebView $(dbType) $(if parameterizedWithDb then appT tpType dbType else tpType) where
       mapWebView = $(lamExp) |]
 where tpType = conT tpName
       dbType = case mDbName of
                   Nothing     -> varT $ mkName "db" 
                   Just dbName -> conT dbName
       
       lamExp = lamE [varP $ mkName "x"] caseExp
 
       caseExp = 
        do { constrs <- getConstructors "deriveMapWebView" tpName
           ; alts <- mapM caseAlt constrs
           ; c <- caseE (varE $ mkName "x") $ alts
           ; return c
           }
       
       caseAlt constr =
        let (cName, cArity) = conNameArity constr
            pattern = mkPattern cName cArity
            body = normalB $ mkApplicativeExpr cName cArity
        in return $ match pattern body []
          
       conNameArity (NormalC name fields) = (name, length fields)
       conNameArity (RecC name fields) = (name, length fields)  
       conNameArity (InfixC _ name _) = (name, 2)
       conNameArity (ForallC _ _ con) = conNameArity con

       -- e.g. mkPattern Cons 3 = Cons x1 x2 x3
       mkPattern name nrOfArgs = conP name [ varP . mkName $ "x"++show i | i <- [1..nrOfArgs] ]
       
       -- e.g. mkApplicativeExpr Cons 3 = pure Cons <*> mapWebView x1 <*> mapWebView x2 <*> mapWebView x3
       mkApplicativeExpr :: Name -> Int -> ExpQ
       mkApplicativeExpr name 0 = [|pure $(conE name)|]
       mkApplicativeExpr name n = [|$(mkApplicativeExpr name $ n-1) <*> mapWebView $(varE . mkName $ "x"++show n)|]

getConstructors tag name = 
 do { info <- reify name 
    ; return $ case info of
                 TyConI (DataD _ _ _ constrs@(_:_) _) -> constrs
                 TyConI (DataD _ _ _ _ _)             -> error $ tag ++ " " ++ show name ++ ": Generics not supported for empty constructors"
                 TyConI _                             -> error $ tag ++ " " ++ show name ++ ": Generics only supported for data types"
                 _                                    -> error $ tag ++ " " ++ show name ++ ": Generics only supported for plain types"
    }
-- NOTE: newtypes could be supported, if necessary                    