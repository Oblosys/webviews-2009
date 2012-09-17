{-# LANGUAGE DeriveDataTypeable, PatternGuards, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, TupleSections, FlexibleInstances, ScopedTypeVariables #-}
module WebViewLibExp where
{- Module for experimenting with generic WebViews that will be put in WebViewLib.
   Keeping them here during development prevents having to recompile the library on every change. 
-}

import Data.Generics
import Data.List
import Data.Function (on)
import Types
import ObloUtils
import Generics
import WebViewPrim
import WebViewLib
import BlazeHtml
import HtmlLib
import TemplateHaskell
import Control.Category hiding (Category) -- fclabels
import Data.Label                         -- fclabels
import Prelude hiding ((.), id)           -- fclabels
import Debug.Trace

data SortView db = 
  SortView (Widget (SelectView db)) (Widget (SelectView db)) [WebView db]  
    deriving (Eq, Show, Typeable, Data)

instance Data db => Initial (SortView db) where
  initial = SortView initial initial initial

deriveMapWebView ''SortView

instance Data db => Storeable db (SortView db)

-- [("sorteer oplopend", id), ("sorteer aflopend", reverse)]
-- [("Sorteer op naam", 
mkSortView :: Data db => [(String, a->a->Ordering)] -> (a-> WebViewM db (WebView db)) -> [a] -> WebViewM db (WebView db)
mkSortView namedSortFunctions mkResultWV results = mkWebView $
  \vid oldView@(SortView sortFieldSelectOld sortOrderSelectOld _) ->
    do { let sortField = getSelection sortFieldSelectOld
       ; let sortOrder = getSelection sortOrderSelectOld
       ; let (sortFieldNames, sortFunctions) = unzip namedSortFunctions
       ; sortFieldSelect <- mkSelectView sortFieldNames sortField True
       ; sortOrderSelect <- mkSelectView ["Oplopend", "Aflopend"] sortOrder True
       
       ; resultsWVs <- sequence [ fmap (r,) $ mkResultWV r | r <- results ]
       ; sortedResultViews <- case results of
                                [] -> fmap singleton $ mkHtmlView $ "Geen resultaten"
                                _  -> return $ map snd $ (if sortOrder == 0 then id else reverse) $ 
                                                         sortBy (sortFunctions !! sortField `on` fst) $ resultsWVs
    
       ; return $ SortView sortFieldSelect sortOrderSelect sortedResultViews
       }

instance Presentable (SortView db) where
  present (SortView sortFieldSelect sortOrderSelect webViews) = 
    (vList $ hStretchList [space, E $ "Sorteer" +++ nbsp, E $ present sortOrderSelect, E $ nbsp +++ "op" +++ nbsp, E $ present sortFieldSelect] 
              ! style "margin: 4 0 4 0"
            : intersperse hSep (map present webViews)
    ) ! style "width: 100%"        
   where hSep = div_ ! style "width: 100%; height:1px; background-color: black; margin: 5 0 5 0" $ noHtml
 
 

data SearchView db = 
  SearchView String (Widget (TextView db)) (Widget (Button db)) (WebView db) String 
    deriving (Eq, Show, Typeable, Data)

instance Data db => Initial (SearchView db) where
  initial = SearchView initial initial initial initial initial

instance Data db =>  MapWebView db (SearchView db) where
  mapWebView (SearchView a b c d e) = SearchView <$> mapWebView a <*> mapWebView b <*> mapWebView c <*> mapWebView d <*> mapWebView e

instance Data db => Storeable db (SearchView db)

-- todo: different languages 
mkSearchView label argName resultsf = mkWebView $
  \vid oldView@( SearchView _ _ _ _ _) ->
    do { args <- getHashArgs
       ; let searchTerm = case lookup argName args of 
                            Nothing    -> ""
                            Just term -> term 
       ; searchField <- mkTextField searchTerm `withTextViewSubmit` (Edit $ return ()) 
       ; searchButton <- mkButtonWithClick "Zoek" True $ const ""
       ; results <- resultsf searchTerm
       ; return $ SearchView label searchField searchButton results $
                  jsScript $
                    let navigateAction = "setHashArg('"++argName++"', "++jsGetWidgetValue searchField++");"
                    in  [ inertTextView searchField
                        , onClick searchButton navigateAction
                        , onSubmit searchField navigateAction
                        ]
       }

instance Presentable (SearchView db) where
  present (SearchView label searchField searchButton wv script) =
      (hStretchList [E $ toHtml label +++ nbsp, Stretch $ with [style "width: 100%;"] (present searchField), E $ present searchButton]) +++
      present wv
      +++ mkScript script

------ Editable Properties (will move to lib)
{-      
-- An encapsulated database update that can be part of a webview. 
data Function a b = Function (a -> b) deriving (Data, Typeable)


instance Initial (Function a b) where
  initial = Function $ error "Initial Function"
  
-- never equal, so no incrementality
instance Eq (Function a b) where
  _ == _ = False
  
instance Show (Function a b) where
  show _ = "Function"
-}
{-
-- Does this explicit instance prevent the need for (Data a, Data b)?
instance Data (Function a b) where
  gfoldl k z (Function f) = z Function `k` f
     
  gunfold k z c = error "gunfold not defined for Function"
     
  toConstr (Function _) = con_Function 
  dataTypeOf _ = ty_Function

ty_Function = mkDataType "Main.Function" [con_Function] -- todo: change according to module
con_Function = mkConstr ty_WebView "Function" [] Prefix
-}
-- non-optimal way to show editable properties. The problem is that the update specified is not a view update but a database update.
data Property db a = EditableProperty (Either Html (PropertyWidget db))
                   | StaticProperty Html deriving (Eq, Show, Typeable, Data)

-- We want to put properties in a list, so an extra parameter for the widget is not an option.
-- We could use an existential, but then deriving instances won't work anymore, so for now we use an explicit sum type.
data PropertyWidget db = PropertyTextView (Widget (TextView db))
                       | PropertySelectView (Widget (SelectView db)) deriving (Eq, Show, Typeable, Data)
                    
instance Data Html
-- TODO: make sure that Data is not actually needed, or implement it.

instance Initial Html where
  initial = noHtml

instance MapWebView db Html
  
instance Initial (Property db a) where
  initial = StaticProperty initial

deriveMapWebView ''PropertyWidget 

-- extra arg, so no derive
instance MapWebView db (Property db a) where
  mapWebView (EditableProperty a) = EditableProperty <$> mapWebView a 
  mapWebView (StaticProperty a) = StaticProperty <$> mapWebView a 
  

{- The update is a database update, but it would be better to be able to specify a view update, since we don't 
want to commit all textfields immediately to the database. Maybe save could be part of the edit monad? (but then do we need
to save again after performing the viewEdit in save?) or we could add an edit action to text fields (not the commit action, but
a blur action) -}
-- not a web view, but it is an instance of Presentable
mkEditableProperty :: (Data db, Show v, Data v, Data a) => 
                      ViewId -> Bool -> (v :-> Maybe a) -> (a :-> p) ->
                      (p -> String) -> (String -> Maybe p) -> (p -> Html) -> a -> 
                      WebViewM db (Property db a)
mkEditableProperty vid editing objectLens valueLens presStr parseStr pres orgObj =
 do { eValue <- if editing
                then fmap (Right . PropertyTextView) $ mkTextFieldWithChange (presStr $ get valueLens orgObj) $ \str ->
                       Edit $ viewEdit vid $ \v ->
                         case get objectLens v of
                           Nothing -> v
                           Just o  -> case parseStr str of
                                        Nothing -> trace ("Parse error for " ++ show str) v
                                        Just p' -> let v' = set objectLens (Just $ set valueLens p' o) v
                                                   in  trace ("Setting "++show vid++" to " ++ show v') $ v'
                else return $ Left $ pres $ get valueLens orgObj 
    ; return $ EditableProperty eValue
    }

mkEditableSelectProperty :: (Data db, Show v, Data v, Data a) => ViewId -> Bool -> (v :-> Maybe a) -> (a :-> p) ->
                            (p -> String) -> (p -> Html) -> [p] -> a ->
                            WebViewM db (Property db a)
mkEditableSelectProperty vid editing objectLens valueLens presStr pres propVals orgObj =
 do { eValue <- if editing
                then let propValStrs = map presStr propVals
                         selection = get valueLens orgObj 
                         -- select index based on string representation, so we don't need Eq on p. (if p's have same string repr. the user won't be able to distinguish anyway)
                         selectionIx = case elemIndex (presStr selection) propValStrs of
                                         Just i  -> i
                                         Nothing -> 0 -- if the property is not in the list, we select the first one 
                                                      -- (this can happen if the list does not contain all values)
                     in  fmap (Right . PropertySelectView) $ mkSelectViewWithChange propValStrs selectionIx True $ \sel -> 
                           Edit $ viewEdit vid $ \v ->
                             case get objectLens v of
                               Nothing -> v
                               Just o  -> if (sel >= 0 && sel < length propVals) 
                                          then set objectLens (Just $ set valueLens (propVals!!sel) o) v
                                          else error $ "Internal error: mkEditableSelectProperty: index " ++ show sel ++
                                                       " out of bounds for: " ++ show (map presStr propVals)
                else return $ Left $ pres $ get valueLens orgObj 
    ; return $ EditableProperty eValue
    }

mkStaticProperty :: (a :-> p) -> (p -> Html) -> a -> WebViewM db (Property db a) -- monadic only to have behave similar to
mkStaticProperty lens pres obj = return $ StaticProperty $ pres $ get lens obj  -- mkEditbleProperty (maybe not necessary)

instance Presentable (Property db a) where
  present (StaticProperty htmlStr)             = toHtml htmlStr
  present (EditableProperty (Left htmlStr))    = toHtml htmlStr
  present (EditableProperty (Right (PropertyTextView textField)))     = present textField
  present (EditableProperty (Right (PropertySelectView selectField))) = present selectField

instance Storeable db (Property db a)

presentEditableProperties :: [(String, Property db a)] -> Html            
presentEditableProperties namedProps =
  table $ sequence_ [ tr $ sequence_ [ td $ with [style "font-weight: bold"] $ toHtml propName, td $ nbsp +++ ":" +++ nbsp
                                     , td $ present prop] 
                    | (propName, prop) <- namedProps
                    ]

------ End of editable Properties
