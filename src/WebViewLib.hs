{-# LANGUAGE CPP #-}
{-# OPTIONS -XDoRec -XDeriveDataTypeable -XFlexibleInstances -XMultiParamTypeClasses -XScopedTypeVariables #-}
module WebViewLib where

import Control.Monad.State
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
import Debug.Trace
import Types
import Generics
import HtmlLib
import Control.Monad.Fix


-- IDEA: use phantom types to enforce type structure on views
--       GADTs?
-- TODO: use a monad for auto handling listeners to database

{-

initials really necessary?
Related: When will oldview not be found during loadView, leading to using oldview' from the webView?
Right now, initials have no viewId, so using them is dangerous.


authenticate shows possible danger. if the refs are evaluated to get the name and password, and the structure
has changed, then the refs will fail. This should not lead to an error. It will probably never happen
since restructuring leads to recreation of the login view because of failure to reuse.

TODO when doing loading on non-root, check how this works with the unique id stuff.

TODO check noViewId, this is now a rootViewId
TODO change liftS

TODO somehow ensure that widgets/subviews are not conditional
TODO unpresented widgets should not crash



Ideas: search pigs by name
inView attr that sets the scrollbars (also nested) so elt with attr is in view
save scroll state of all scrollers, and restore after reload
separate specific from generic part
tabbed views, also with separate urls
templates from files in presentation
monad for editing
cleanup
-}

-- TODO: Maybe put this in a Utils?
debugLn str = trace str $ return ()

-- When loading from different point than root, make sure Id's are not messed up

instance Data db => Storeable db (WebView db) where
  save (WebView _ _ _ _ v) =
    let topLevelWebViews :: [WebView db] = getTopLevelWebViews v
    in  foldl (.) id $ save v : map save topLevelWebViews



---- Monad stuff

runWebView user db viewMap path viewIdCounter wvm =
 do { rv <- evalStateT wvm (WebViewState user db viewMap path viewIdCounter)
    ; return $ assignIds rv
    }


withDb f = do { (WebViewState _ db _ _ _) <- get
              ; return $ f db
              }


getUser :: WebViewM db User 
getUser = do { (WebViewState u _ _ _ _) <- get
             ; return u
             }


liftS :: ([Int] -> Int -> (x,Int)) -> WebViewM db x
liftS f = StateT (\(WebViewState user db viewMap path i) -> 
                   (return $ let (x, i')= f path i in (x, WebViewState user db viewMap path i')))




mkButton :: String -> Bool -> EditCommand db -> WebViewM db (Widget (Button db))
mkButton str en ac = mkButtonWithStyle str en "" ac

mkButtonWithStyle :: String -> Bool -> String -> EditCommand db -> WebViewM db (Widget (Button db))
mkButtonWithStyle str en st ac = liftS $  \path vidC -> (button (ViewId $ path ++ [vidC]) str en st ac, vidC + 1)

-- no need to be in monad
mkEditAction :: EditCommand db -> WebViewM db (EditAction db) 
mkEditAction ec = liftS $ \path vidC -> (EditAction (ViewId $ path ++ [vidC]) ec, vidC +1)

mkRadioView is s en = liftS $ \path vidC -> (radioView (ViewId $ path ++ [vidC]) is s en, vidC +1)


mkTextField str = mkTextFieldEx str Nothing

mkTextFieldAct str act = mkTextFieldEx str $ Just act

mkTextFieldEx str mEditAction = liftS $ \path vidC -> (textField (ViewId $ path ++ [vidC]) str mEditAction, vidC + 1)

mkPasswordField str = mkPasswordFieldEx str Nothing

mkPasswordFieldAct str act = mkPasswordFieldEx str $ Just act

mkPasswordFieldEx str mEditAction = liftS $ \path vidC -> (passwordField (ViewId $ path ++ [vidC]) str mEditAction , vidC + 1)

mkTextArea str = liftS $ \path vidC -> (textArea (ViewId $ path ++ [vidC]) str, vidC + 1)




widgetGetViewRef (Widget _ _ w) = mkViewRef $ getViewId w
                  

{-
mkAction :: (v->v) -> ViewM v Int
mkAction f = 
 do { actions <- get
    ; put $ actions ++ [f]
    ; return $ length actions + 1
    }


testMonad = print $ "bla" {- runMonad [1,2,3] $
 do { l <- getList
    ; return l
    } -}

-}
-- hide this constructor, so user needs to use mkView
{-
data AView v = AView { editActions :: [v->v], makeView :: v -> v }

mkView :: (ViewM v (v->v)) -> (AView v)
mkView mv = runIdentity $ 
 do { (mkview, state) <- runStateT mv []
    ; return $ AView state mkview
    }



data TestView = TestView Int String

--mkTestView
mkTestView v = mkView $ 
 do { i <- mkAction $ \(TestView x s) -> TestView x s
    ; return $ \v -> TestView i "bla"
    } 
-}


--- Edit Monad


docEdit :: (db -> db) -> EditM db ()
docEdit docUpdate =
 do { (sessionId, user, db, rootView, pendingEdit) <- get
    ; put (sessionId, user, docUpdate db, rootView, pendingEdit)
    }

viewEdit :: (Typeable db, Data db, Data v) => ViewId -> (v -> v) -> EditM db ()
viewEdit vid viewUpdate =
  do{ (sessionId, user, db, rootView, pendingEdit) <- get
    ; let webViewUpdate = \(WebView vi si i lv v) ->
                            WebView vi si i lv $ applyIfCorrectType viewUpdate v
          wv = getWebViewById vid rootView
          wv' = webViewUpdate wv
          rootView' = replaceWebViewById vid wv' rootView 
                          
    ; put (sessionId, user, save rootView' db, rootView', pendingEdit)
    }

-- TODO save automatically, or maybe require explicit save? (after several view updates)
-- for now, just after every viewEdit

applyIfCorrectType :: (Typeable y, Typeable x) => (y -> y) -> x -> x
applyIfCorrectType f x = case cast f of 
                           Just fx -> fx x
                           Nothing -> x

-- Experimental, not sure if we need this one and if it works correctly
withView :: forall db v a . (Typeable db, Data db, Data v, Typeable a) => ViewId -> (v->a) -> EditM db (Maybe a)
withView vid f =
  do{ (sessionId, user, db, rootView, pendingEdit) <- get
    ; let wf = \(WebView vi si i lv v) -> case cast f of
                                            Nothing -> Nothing
                                            Just castf -> Just $ castf v
          wv = getWebViewById vid rootView :: WebView db
          
    ; return $ wf wv
    }

-- the view matching on load can be done explicitly, following structure and checking ids, or
-- maybe automatically, based on id. Maybe extra state can be in a separate data structure even,
-- like in Proxima
loadView :: WebView db -> WebViewM db (WebView db)
loadView (WebView _ si i mkView oldView') =
 do { state@(WebViewState user db viewMap path viewIdCounter) <- get
    ; let newViewId = ViewId $ path ++ [viewIdCounter]                             
    ; put $ WebViewState user db viewMap (path++[viewIdCounter]) 0
    ; let oldView = case lookupOldView newViewId viewMap of
                      Just oldView -> oldView
                      Nothing      -> oldView' -- this will be initial
                      
    ; view <- mkView newViewId oldView -- path is one level deeper for children created here 
    ; modify $ \s -> s { getStatePath = path, getStateViewIdCounter = viewIdCounter + 1} -- restore old path
    ; return $ WebView newViewId si i mkView view    
    }
mkWebView :: Data db => (Presentable v, Storeable db v, Initial v, Show v, Eq v, Data v) =>
             (ViewId -> v -> WebViewM db v) ->
             WebViewM db (WebView db)
mkWebView mkView  =
 do { let initialWebView = WebView noViewId noId noId mkView initial
    ; webView <- loadView initialWebView
    ; return webView
    } 


{-
id that is unique in parent guarantees no errors. What about uniqueness for pigs when switching visits?
what about space leaks there?

TODO: this seems only necessary for subviews, as these can have extra state
for widgets, there may be a little less reuse, but no problem. Except maybe if a widget
is created based on a list of elements, but does not take its content from these elements.

nice to do it in pig itself, but then we need to remove it's ols unique id from the path and make sure
that it's own unique id does not clash with generated id's or unique id's from other lists. We cannot
include the generated unique id, because then the unique id does not identify the view by itself anymore

widget [.., 0]
widget [.., 1]
views  [.., unique1]
       [.., unique2]
view   [.., 2]
views  [.., unique3]
views  [.., unique4]
(uniques may not overlap with {0,1,2})

doing it at the list is easier

widget [.., 0]
widget [.., 1]
views  [..,2, unique1]
       [..,2, unique2]
view   [.., 3]
views  [.., 4,unique1]
views  [.., 4,unique2]

Now the unique id's only have to be unique with regard to other elts in the list.

Note that the zeros after the uniques are added due to the way mkWebview and load Webview work.
-}
uniqueIds :: [(Int, WebViewM db (WebView db))] -> WebViewM db [WebView db]
uniqueIds idsMkWvs =
 do { state@(WebViewState user db viewMap path viewIdCounter) <- get
    ; put $ state { getStatePath = path++[viewIdCounter], getStateViewIdCounter = 0 } 
      -- 0 is not used, it will be replaced by the unique ids
      
    ; wvs <- sequence [ uniqueId id mkWv | (id, mkWv) <- idsMkWvs]
    ; modify $ \s -> s { getStatePath = path, getStateViewIdCounter = viewIdCounter+1 }
    ; return wvs
    }
 
uniqueId uniqueId wv =
 do { modify $ \s -> s { getStateViewIdCounter = uniqueId } 
    ; v <- wv
    ; return v
    }

{-

Everything seems to work:
not updating the unchanged controls and keeping id's unique causes edit events survive update
They do seem to lose focus though, but since we know what was edited, we can easily restore the focus after
the update
-}


-- textfields are in forms, that causes registering text field updates on pressing enter
-- (or Done) on the iPhone.

presentTextField :: Text db -> Html
presentTextField (Text viewId TextArea str _) = 
   form![ thestyle "display: inline; width: 500px;"
        , strAttr "onSubmit" $ "textFieldChanged('"++show viewId++"'); return false"] $
     textarea ! [ identifier (show viewId)
                , thestyle "width: 100%; height: 100%;"
                --, strAttr "onChange" $ "textFieldChanged('"++show i++"')"
                , strAttr "onFocus" $ "elementGotFocus('"++show viewId++"')"
                , strAttr "onBlur" $ "textFieldChanged('"++show viewId++"')"
                ] << stringToHtml str
presentTextField (Text viewId textType str mEditAction) = 
  let inputField = case textType of TextField -> textfield ""
                                    PasswordField -> password ""
                                    
  in form![ thestyle "display: inline"
          , strAttr "onSubmit" $ "textFieldChanged('"++show viewId++"');" ++
                                 (case mEditAction of
                                    Nothing -> []
                                    Just _  -> "queueCommand('SubmitC ("++show viewId++")'); ")++
                                 "return false"] $
       inputField ! [ identifier (show viewId), strAttr "value" str, width "100%"
                    --, strAttr "onChange" $ "textFieldChanged('"++show i++"')"
                    , strAttr "onFocus" $ "elementGotFocus('"++show viewId++"')"
                    , strAttr "onBlur" $ "textFieldChanged('"++show viewId++"')" ]

-- seems like this one could be in Present
presentButton :: Button db -> Html
presentButton (Button viewId txt enabled style _) = 
   primHtml $ "<button id=\""++ show viewId++"\" "++ (if enabled then "" else "disabled ") ++ (if style /="" then " style=\"" ++style++"\" " else "")++
                            "onclick=\"queueCommand('ButtonC ("++show viewId++")')\" "++
                            "onfocus=\"elementGotFocus('"++show viewId++"')\">"++txt++"</button>"
-- TODO: text should be escaped


-- Edit actions are a bit different, since they do not have a widget presentation.
-- TODO: maybe combine edit actions with buttons, so they use the same command structure
-- a descriptive text field can be added to the action, to let the server be able to show
-- which button was pressed. However, it is not sure if this works okay with restoring id's
-- though it probably works out, as the ea id is the only one needing restoration.
withEditAction (EditAction viewId _) elt = 
  thespan![ identifier $ show viewId
          , strAttr "onClick" $ "queueCommand('PerformEditActionC ("++show viewId++")')"] << elt

withEditActionAttr (EditAction viewId _) = 
  strAttr "onClick" $ "queueCommand('PerformEditActionC ("++show viewId++")')"

presentRadioBox :: RadioView -> Html
presentRadioBox (RadioView viewId items selectedIx enabled) = thespan << 
  [ radio (show viewId) (show i) ! ( [ identifier eltId 
                          , strAttr "onChange" ("queueCommand('SetC ("++show viewId++") %22"++show i++"%22')") 
                          , strAttr "onFocus" ("elementGotFocus('"++eltId++"')")
                          ]
                          ++ (if enabled && i == selectedIx then [strAttr "checked" ""] else []) 
                          ++ (if not enabled then [strAttr "disabled" ""] else [])) 
                          +++ item +++ br 
  | (i, item) <- zip [0..] items 
  , let eltId = "radio"++show viewId++"button"++show i ] -- these must be unique for setting focus

instance Presentable (WebView db) where
  present (WebView _ (Id stubId) _ _ _) = mkSpan (show stubId) << "ViewStub"
  
instance Presentable (Widget x) where
  present (Widget (Id stubId) _ _) = mkSpan (show stubId) << "WidgetStub"


-- todo button text and radio text needs to go into view
instance Presentable (AnyWidget db) where                          
  present (RadioViewWidget rv) = presentRadioBox rv 
  present (TextWidget es) = presentTextField es 
  present (ButtonWidget b) = presentButton b 
  present (EditActionWidget b) = noHtml



-- Login -----------------------------------------------------------------------  

data LoginView db = LoginView (Widget (Text db)) (Widget (Text db)) (Widget (Button db)) 
  deriving (Eq, Show, Typeable, Data)

instance Initial (LoginView db) where initial = LoginView initial initial initial

mkLoginView :: Data db => WebViewM db (WebView db)
mkLoginView = mkWebView $
  \vid (LoginView name password b) ->
#if __GLASGOW_HASKELL__ >= 612
    do { rec { nameT <- mkTextFieldAct (getStrVal name) authenticate 
             ; passwordT <- mkPasswordFieldAct (getStrVal password) authenticate 
             ; let authenticate = AuthenticateEdit (widgetGetViewRef nameT) (widgetGetViewRef passwordT)
             }
#else
    mdo { nameT <- mkTextFieldAct (getStrVal name) authenticate 
        ; passwordT <- mkPasswordFieldAct (getStrVal password) authenticate 
        ; let authenticate = AuthenticateEdit (widgetGetViewRef nameT) (widgetGetViewRef passwordT)
#endif

       ; loginB <- mkButton "Login" True authenticate                   
       ; return $ LoginView nameT passwordT loginB
       }

instance Storeable db (LoginView db) where save _ = id
                                   
instance Presentable (LoginView db) where
  present (LoginView name password loginbutton) = 
    boxed $ simpleTable [] [] [ [ stringToHtml "Login:", present name]
                              , [ stringToHtml "Password:", present password]
                              , [ present loginbutton ]
                              ]
            


-- Logout ----------------------------------------------------------------------  

data LogoutView db = LogoutView (Widget (Button db)) deriving (Eq, Show, Typeable, Data)

instance Initial (LogoutView db) where initial = LogoutView initial

mkLogoutView :: Data db => WebViewM db (WebView db)
mkLogoutView = mkWebView $
  \vid _ -> 
   do { (Just (l,_)) <- getUser
      ; logoutB <- mkButton ("Logout " ++  l) True LogoutEdit
      ; return $ LogoutView logoutB
      }
instance Storeable db (LogoutView db) where save _ = id
                                   
instance Presentable (LogoutView db) where
  present (LogoutView logoutbutton) = 
    present logoutbutton
            


-- LinkView ---------------------------------------------------------------------  

-- This is a separate view for editActions. Putting edit actions inside a view that is changed
-- may cause press events to get lost. This indirection solves the problem.
data LinkView db = LinkView String (EditAction db) deriving (Eq, Show, Typeable, Data)

instance Initial (LinkView db) where initial = LinkView initial initial

mkLinkView linkText action = mkWebView $
  \vid _ ->
   do { editAction <- mkEditAction action
      ; return $ LinkView linkText editAction
      }
   
instance Storeable db (LinkView db) where save _ = id

instance Presentable (LinkView db) where
  present (LinkView linkText editAction) = withEditAction editAction $ stringToHtml linkText



-- TabbedView ---------------------------------------------------------------------  
  
data TabbedView db = TabbedView Int [WebView db] [WebView db] deriving (Eq, Show, Typeable, Data)

instance Initial (TabbedView db) where
  initial = TabbedView 0 initial initial

mkTabbedView :: forall db . Data db => [(String, Maybe (EditM db ()), WebView db)] -> WebViewM db (WebView db)
mkTabbedView labelsEditActionsTabViews = mkWebView $
 \vid (TabbedView selectedTab _ _) ->
  do { let (labels, mEditActions,tabViews) = unzip3 labelsEditActionsTabViews
           
     ; selectionViews <- sequence [ mkLinkView label $ Edit $ 
                                     do { viewEdit vid $
                                            \((TabbedView _ sas twvs) :: TabbedView db) -> TabbedView i sas twvs
                                        ; case mEditAction of
                                            Nothing -> return ()
                                            Just ea -> ea
                                        }
                                  | (i, label, mEditAction) <- zip3 [0..] labels mEditActions
                                  ]
     ; return $ TabbedView selectedTab selectionViews tabViews
     }
  
instance Data db => Storeable db (TabbedView db) where
  save (TabbedView _ _ tabViews) = foldl (.) id $ map save tabViews

-- TODO: may have been broken by new roundedBoxed implementation
instance Presentable (TabbedView db) where
  present (TabbedView selectedTab selectionViews tabViews) = 
    hList [ thespan![ theclass "tab"
                    , thestyle ("background-color: "++color)
                    ] $ present selectionView 
          | (i,selectionView) <- zip [0..] selectionViews
          , let color = htmlColor $ if i == selectedTab then Color white else Rgb 200 200 200
          ] +++
    (roundedBoxed (Just $ Color white) $
     concatHtml [ thediv![attr] $ present tabView 
                | (i,tabView) <- zip [0..] tabViews 
                , let attr = thestyle $ "display: " ++ if i == selectedTab 
                                                       then "visible"
                                                       else "none"
                ])

{- version that uses jQuery tabs. Does weird things with font and buttons
instance Presentable TabbedView where
  present (TabbedView _ tabViews) = 
    thediv![theclass "tabbed"] <<
      ((ulist $ concatHtml [li $ anchor![href $ "#"++escapeId webView] $ stringToHtml label 
                           | (webView,label)  <- zip tabViews ["een","twee","drie"] ] ) +++
       (concatHtml [ mkDiv (escapeId tabView) $ present tabView | tabView <- tabViews ] ))

escapeId wv = let ViewId path = getViewId wv
              in  [ if c `elem` "[,]" then '-' else c | c <- show path ]
-}