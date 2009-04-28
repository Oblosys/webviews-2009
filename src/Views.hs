{-# OPTIONS -fglasgow-exts #-}
module Views where

import Control.Monad.Trans
import Data.List
import Text.Html hiding (image)
import qualified Text.Html as Html
import Data.Generics
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Data.Tree

import Types
import Database
import Generics

-- IDEA: use phantom types to enforce type structure on views
--       GADTs?
-- TODO: figure out load stuff
--       use a monad for auto handling listeners to database


instance Storeable WebView where
  save (WebView _ _ _ _ v) =
    let topLevelWebViews = getTopLevelWebViews v
    in  foldl (.) id $ save v : map save topLevelWebViews

  
-- first save self, then children
-- TODO: does this order matter?

-- should be with TH stuff for Database, like DatabaseUtils or something
instance Initial VisitId where initial = VisitId (-1)

instance Initial PigId where initial = PigId (-1)



mkViewEdit i f = 
  ViewEdit i $ \(WebView vi si i lv v) -> WebView vi si i lv $ applyIfCorrectType f v


applyIfCorrectType :: (Typeable y, Typeable x) => (y -> y) -> x -> x
applyIfCorrectType f x = case cast f of 
                           Just fx -> fx x
                           Nothing -> x


-- the view matching on load can be done explicitly, following structure and checking ids, or
-- maybe automatically, based on id. Maybe extra state can be in a separate data structure even,
-- like in Proxima
mkRootView :: User -> Database -> Int -> ViewMap -> WebView
mkRootView user db sessionId viewMap = 
  --mkPigView 3 (PigId 1) 5 user db viewMap 
  mkVisitView sessionId (VisitId 1) user db viewMap

mkWebView :: (Presentable v, Storeable v, Initial v, Show v, Eq v, Data v) =>
             ViewId -> (User -> Database -> ViewMap -> ViewId -> v) -> User -> Database -> ViewMap -> WebView
mkWebView vid wvcnstr user db viewMap = loadView user db viewMap $
  WebView vid noId noId wvcnstr initial

loadView user db viewMap (WebView vid si i f _) = (WebView vid si i f (f user db viewMap vid))



data VisitView = 
  VisitView VisitId Int User EString EString EInt Button Button Button [PigId] [String] WebView [WebView]
    deriving (Eq, Show, Typeable, Data)

-- todo: doc edits seem to reset viewed pig nr.
mkVisitView sessionId i = mkWebView (ViewId 0) $
  \user db viewMap vid -> 
  let (VisitView _ _ _ _ _ oldViewedPig _ _ _ _ _ _ mpigv) = getOldView vid viewMap
      (Visit visd zipcode date pigIds) = unsafeLookup (allVisits db) i
      viewedPig = constrain 0 (length pigIds - 1) $ getIntVal oldViewedPig
      pignames = map (pigName . unsafeLookup (allPigs db)) pigIds
  in  VisitView i sessionId user (estr zipcode) (estr date) (eint viewedPig) 
                (Button noId (previous (ViewId 0))) (Button noId (next (ViewId 0)))
                (Button noId (addPig visd)) pigIds pignames
-- todo: check id's
             {-   (if null pigIds -- remove guard for weird hanging exception (after removing last pig)
                    then Nothing
                    else Just $ mkPigView viewedPig (pigIds !! viewedPig) db viewMap
                   )-}
                (if user == Nothing then (mkLoginView user db viewMap) 
                                    else (mkLogoutView user db viewMap))
                [mkPigView i pigId viewedPig user db viewMap | (pigId,i) <- zip pigIds [0..]]
 where -- next and previous may cause out of bounds, but on reload, this is constrained
       previous i = mkViewEdit i $
         \(VisitView vid sid us zipCode date viewedPig b1 b2 b3 pigs pignames loginv mSubview) ->
         VisitView vid sid us zipCode date (eint $ getIntVal viewedPig-1) b1 b2 b3 pigs pignames loginv mSubview

       next i = mkViewEdit i $
         \(VisitView vid sid us zipCode date viewedPig b1 b2 b3 pigs pignames loginv mSubview) ->
         VisitView vid sid us zipCode date (eint $ getIntVal viewedPig+1) b1 b2 b3 pigs pignames loginv mSubview

       addPig i = DocEdit $ addNewPig i 

addNewPig vid db =
  let ((Pig newPigId _ _ _ _), db') = newPig vid db      
  in  (updateVisit vid $ \v -> v { pigs = pigs v ++ [newPigId] }) db'

instance Presentable VisitView where
  present (VisitView vid sid us zipCode date viewedPig b1 b2 b3 pigs pignames loginoutView subviews) =
        withBgColor (Rgb 235 235 235) $
        (case us of
           Nothing -> p << present loginoutView 
           Just (_,name) -> p << stringToHtml ("Hello "++name++".") +++ present loginoutView) +++
        
        (p <<"Visit at "+++ presentTextField zipCode +++" on " +++ presentTextField date +++ 
          "           (session# "+++show sid+++")")
    +++ p << ("Visited "++ show (length pigs) ++ " pig" ++ pluralS (length pigs) ++ ": " ++
              listCommaAnd pignames)
    +++ p << ((if null pigs then stringToHtml $ "Not viewing any pigs" 
               else "Viewing pig nr. " +++ show (getIntVal viewedPig) ++ "   ")
    -- "debugAdd('boing');queueCommand('Set("++id++","++show i++");')"
           +++ presentButton "previous" b1 +++ presentButton "next" b2)
    +++ withPad 15 0 0 0 {- (case mSubview of
               Nothing -> stringToHtml "no pigs"
               Just pv -> present pv) -}
            (case subviews of
               [] -> stringToHtml "no pigs"
               pvs -> hList $ map present pvs ++ [presentButton "add" b3] )

instance Storeable VisitView where
  save (VisitView vid sid us zipCode date _ _ _ _ pigs pignames _ _) db =
    updateVisit vid (\(Visit _ _ _ pigIds) ->
                      Visit vid (getStrVal zipCode) (getStrVal date) pigIds)
                    db

instance Initial VisitView where
  initial = VisitView initial initial initial initial initial initial initial initial initial initial initial initial initial

                                 
data PigView = PigView PigId Button Int Int EString [EInt] (Either Int String) 
               deriving (Eq, Show, Typeable, Data)

mkPigView pignr i viewedPig = mkWebView (ViewId $ 10+pignr) $ 
      \user db viewMap vid ->
  let (Pig pid vid name symptoms diagnosis) = unsafeLookup (allPigs db) i
  in  PigView pid (Button noId ( ConfirmEdit ("Are you sure you want to remove pig "++show pignr++"?") $ 
                                   removePigAlsoFromVisit pid vid)) 
              viewedPig pignr (estr name) (map eint symptoms) diagnosis
 where -- need db support for removal and we need parent
       removePigAlsoFromVisit pid vid =
         DocEdit $ removePig pid . updateVisit vid (\v -> v { pigs = delete pid $ pigs v } )  

instance Presentable PigView where
  present (PigView pid b _ pignr name [] diagnosis) = stringToHtml "initial pig"
  present (PigView pid b viewedPig pignr name [tv, kl, ho] diagnosis) =
        withBgColor (if viewedPig == pignr then Rgb 200 200 200 else Rgb 225 225 225) $
     mkSpan ("pigview"++show (unId (getStrId name))) $ -- borrow from name, so id is changed by syb 
     boxed $
        (center $ image $ pigImage)
    +++ (center $ " nr. " +++ show (pignr+1))
    +++ p << (center $ (presentButton "remove" b))
    +++ p << ("Name:" +++ presentTextField name)
    +++ p << "Type varken: " 
    +++ presentRadioBox ["Roze", "Grijs"] tv
    +++ p << "Fase cyclus: "
    +++ presentRadioBox ["1", "2", "3"] kl
    +++ p << "Haren overeind: "
    +++ presentRadioBox ["Ja", "Nee"] ho
    +++ p << ("diagnosis " ++ show diagnosis)
    where pigImage | viewedPig == pignr = "pig.png"
                   | viewedPig < pignr = "pigLeft.png"
                   | viewedPig > pignr = "pigRight.png"

instance Storeable PigView where
  save (PigView pid _ _ _ name symptoms diagnosis) =
    updatePig pid (\(Pig _ vid _ _ diagnosis) -> 
                    (Pig pid vid (getStrVal name) (map getIntVal symptoms) diagnosis)) 

instance Initial PigView where
  initial = PigView initial initial initial initial initial initial initial





data LoginView = LoginView EString EString Button 
  deriving (Eq, Show, Typeable, Data)

mkLoginView = mkWebView (ViewId (44)) $
      \user db viewMap vid ->
        let (LoginView name password b) = getOldView vid viewMap
        in  LoginView name password 
                     (Button noId $ AuthenticateEdit (strRef name) (strRef password))

instance Storeable LoginView where save _ = id
                                   
instance Presentable LoginView where
  present (LoginView name password loginbutton) = 
    boxed $ ("Login:" +++ presentTextField name) +++
            ("Password:" +++ presentPasswordField password) +++
            presentButton "Login" loginbutton
            
instance Initial LoginView where initial = LoginView initial initial initial

data LogoutView = LogoutView Button deriving (Eq, Show, Typeable, Data)

mkLogoutView = mkWebView (ViewId (55)) $
  \user db viewMap vid -> LogoutView (Button noId LogoutEdit)

instance Storeable LogoutView where save _ = id
                                   
instance Presentable LogoutView where
  present (LogoutView logoutbutton) = 
    presentButton "Logout" logoutbutton
            
instance Initial LogoutView where initial = LogoutView initial




updateReplaceHtml :: String -> Html -> Html
updateReplaceHtml targetId newElement =
  thediv![strAttr "op" "replace", strAttr "targetId" targetId ] 
    << newElement

mkDiv str elt = thediv![identifier str] << elt

mkSpan str elt = thespan![identifier str] << elt

boxed html = thediv![thestyle "border:solid; border-width:1px; padding:4px;"] << html



{-

Everything seems to work:
not updating the unchanged controls and keeping id's unique causes edit events survive update
They do seem to lose focus though, but since we know what was edited, we can easily restore the focus after
the update
-}


-- don't use Presentable, because we might present strings in different ways.

-- the entire root is a form, that causes registering text field updates on pressing enter
-- (or Done) on the iPhone. It would be nicer to capture this at the textfield itself.
-- Local forms are a problem though because they are block elements
-- TODO: focus loss on enter is not nice  
presentTextField :: EString -> Html
presentTextField = presentTextualInput (textfield "")
  
presentPasswordField :: EString -> Html
presentPasswordField = presentTextualInput (password "")

presentTextualInput :: Html -> EString -> Html
presentTextualInput inputfield (EString (Id i) str) = mkSpan ("input"++show i) $  
  inputfield ! [identifier (show i), strAttr "VALUE" str
               --, strAttr "onChange" $ "textFieldChanged('"++show i++"')"
               , strAttr "onFocus" $ "elementGotFocus('"++show i++"')"
               , strAttr "onBlur" $ "textFieldChanged('"++show i++"')"
               ]

-- seems like this one could be in Present
presentButton :: String -> Button -> Html
presentButton txt (Button (Id i) _) = mkSpan ("input"++show i) $ 
   primHtml $ "<button onclick=\"queueCommand('ButtonC "++show i++"')\""++
                      "onfocus=\"elementGotFocus('"++show i++"')\">"++txt++"</button>"
-- TODO: text should be escaped

presentRadioBox :: [String] -> EInt -> Html
presentRadioBox items (EInt (Id i) ei) = mkSpan ("input"++show i) $ radioBox (show i) items ei
-- id is unique


--radioBox :: String -> [String] -> Int -> Html
radioBox id items selectedIx =
  [ radio id (show i) ! ( [ strAttr "id" eltId 
                          , strAttr "onChange" ("queueCommand('SetC "++id++" %22"++show i++"%22')") 
                          , strAttr "onFocus" ("elementGotFocus('"++eltId++"')")
                          ]
                          ++ if i == selectedIx then [strAttr "checked" ""] else []) 
                          +++ item +++ br 
                        | (i, item) <- zip [0..] items 
                        , let eltId = "radio"++id++"button"++show i ]


hList [] = stringToHtml "" -- TODO should have some empty here
hList views = simpleTable [] [] [ views ]

vList [] = stringToHtml "" -- TODO should have some empty here
vList views = simpleTable [] [] [ [v] | v <- views ]

data Color = Rgb Int Int Int
           | Color String deriving Show

withBgColor (Rgb r g b) h = let colorStr = "#" ++ toHex2 r ++ toHex2 g ++ toHex2 b
                            in  thediv ! [thestyle $ "background-color: "++ colorStr ++";"] << h
withBgColor (Color colorStr) h = thediv ! [thestyle $ "background-color: "++colorStr++";"] << h

-- Utils

lputStr :: MonadIO m => String -> m ()
lputStr = liftIO . putStr

lputStrLn :: MonadIO m => String -> m ()
lputStrLn = liftIO . putStrLn

constrain mn mx x = (mn `max` x) `min` mx

toHex2 :: Int -> String
toHex2 d = [toHexDigit $ d `div` 16] ++ [toHexDigit $ d `mod` 16]

toHexDigit d = let d' = constrain 0 15 d
               in  chr $ d' + if d < 10 then ord '0' else ord 'A' - 10  

withPad left right top bottom h =
  thediv ! [thestyle $ "padding: "++show top++"px "++show right++"px "++
                       show bottom++"px "++show left++"px;"] << h

image filename = Html.image ! [src $ "/img/"++ filename ]

pluralS 1 = ""
pluralS n = "s" 

listCommaAnd :: [String] -> String
listCommaAnd [] = ""
listCommaAnd [s]  = s
listCommaAnd ss@(_:_) = (concat . intersperse ", " $ init ss) ++ " and " ++ last ss 


data Update = Move IdRef IdRef -- move element target 

-- make sure new rootview has fresh webid's
diffViews :: ViewMap -> WebView -> ([WebView], [Update])
diffViews oldViewMap rootView = 
  let newViewMap = mkViewMap rootView
      newOrChangedIdsViews   = getNewOrChangedIdsViews oldViewMap newViewMap
      (newOrChangedViewIds, newOrChangedViews) = unzip newOrChangedIdsViews     
      combinedViewMap = Map.fromList newOrChangedIdsViews `Map.union` oldViewMap
  in  (newOrChangedViews, computeMoves oldViewMap combinedViewMap newOrChangedViewIds rootView)

-- combined can be queried to get the id for each view at the moment before applying the moves to the
-- tree.

getNewOrChangedIdsViews :: ViewMap -> ViewMap -> [(ViewId, WebView)]
getNewOrChangedIdsViews oldViewMap newViewMap =
  filter isNewOrChanged $ Map.toList newViewMap
 where isNewOrChanged (i, view) =
         case Map.lookup i oldViewMap of
           Nothing -> True
           Just oldView -> oldView /= view


         
computeMoves :: ViewMap -> ViewMap -> [ViewId] -> WebView -> [Update]           
computeMoves oldViewMap combinedViewMap changedOrNewViews rootView = 
  let allViews = getBreadthFirstSubViews rootView
  in  concatMap (computeChildMoves oldViewMap combinedViewMap changedOrNewViews) allViews

-- make a breadth-first list of all views in root view, and for each view,
-- and for each view:
--              get subviews
--              get subviews of previous (or nothing if no previous)
--                 each subview, if previous == nothing or subview /= old subview           
--                               then move subview 
           

webViewGetId (WebView _ _ i _ _) = i 



computeChildMoves :: ViewMap -> ViewMap -> [ViewId] -> WebView -> [Update]           
computeChildMoves oldViewMap combinedViewMap changedOrNewViews (WebView vi _ _ _ view) =
  let childViews = getTopLevelWebViews view
      mOldChildViews = case Map.lookup vi oldViewMap of 
                        Nothing                 -> Nothing
                        Just (WebView _ _ _ _ v)  -> Just $ getTopLevelWebViews v
  in  case mOldChildViews of
        Nothing -> [ Move (IdRef sourceId) (IdRef stubid) | (WebView vid (Id stubid) _ _ _) <- childViews 
                                                    , let (Id sourceId) = getIdForViewWithViewId combinedViewMap vid 
                                                           
                                                    ]
                                                      -- stubId is used as id for stubs when presenting
                   
        Just oldChildViews -> concat
          [ if newViewId == oldViewId && not (newViewId `elem` changedOrNewViews)
            then [] 
            else [Move (IdRef sourceId) (IdRef oldId)]
          | (WebView newViewId _ _ _ _, WebView oldViewId _ (Id oldId) _ _) <-  zip childViews oldChildViews 
          , let (Id sourceId) = getIdForViewWithViewId combinedViewMap newViewId
          ] 
getIdForViewWithViewId :: ViewMap -> ViewId -> Id          
getIdForViewWithViewId combinedViewMap viewId =
   case Map.lookup viewId combinedViewMap of 
     Nothing -> error "bla"
     Just wv -> webViewGetId wv
     
getBreadthFirstSubViews rootView =
  concat $ takeWhile (not . null) $ iterate (concatMap getTopLevelSubViews') [rootView] 
 where getTopLevelSubViews' (WebView _ _ _ _ vw) = getTopLevelWebViews vw
        
-- todo: change present to non-recursive, taking into account stubs
--       put id'd divs around each webview
--       handle root
--       improve id assignment, only assign if not -1
--       assignment ids = getIds; Set.fromList [0..length ids -1] `Set.minus` Set.fromList ids

drawViews webview = drawTree $ treeFromView webview
 where treeFromView (WebView vid sid id _ v) =
         Node ("("++show vid ++ ", stub:" ++ show (unId sid) ++ ", id:" ++ show (unId id) ++ ") : " ++ show (typeOf v)) $
              map treeFromView $ getTopLevelWebViews v
         
data T = T Char [T]
t0 = T 'a' [T 'b' [T 'd' [], T 'e' []], T 'c' [], T 'f' [T 'g' []]]

bfs (T x cs) = [x] :  (map concat $ transpose $ map bfs cs)
