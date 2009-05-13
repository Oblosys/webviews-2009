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
import Database
import Generics
import HtmlLib
import Control.Monad.Fix


-- IDEA: use phantom types to enforce type structure on views
--       GADTs?
-- TODO: figure out load stuff
--       use a monad for auto handling listeners to database

{-

initials really necessary?
Related: When will oldview not be found during loadView, leading to using oldview' from the webView?


-}





-- When loading from different point than root, make sure Id's are not messed up

instance Storeable WebView where
  save (WebView _ _ _ _ v) =
    let topLevelWebViews = getTopLevelWebViews v
    in  foldl (.) id $ save v : map save topLevelWebViews



---- Monad stuff

runWebView user db viewMap path viewIdCounter wvm =
 do { rv <- evalStateT wvm (WebViewState user db viewMap path viewIdCounter)
    ; return $ assignIds rv
    }


withDb f = do { (WebViewState _ db _ _ _) <- get
              ; return $ f db
              }


getUser :: WebViewM User 
getUser = do { (WebViewState u _ _ _ _) <- get
             ; return u
             }


liftS :: ([Int] -> Int -> (x,Int)) -> WebViewM x
liftS f = StateT (\(WebViewState user db viewMap path i) -> 
                   (return $ let (x, i')= f path i in (x, WebViewState user db viewMap path i')))




mkButton :: String -> Bool -> EditCommand -> WebViewM (Widget Button)
mkButton str en ac = liftS $  \path vidC -> (button (ViewId $ path ++ [vidC]) str en ac, vidC + 1)

-- no need to be in monad
mkEditAction ac = return $ EditAction noId ac

mkRadioView is s en = liftS $ \path vidC -> (radioView (ViewId $ path ++ [vidC]) is s en, vidC +1)


mkTextField str = mkTextFieldEx str Nothing

--mkTextFieldAct str act = mkTextFieldEx str $ Just act

mkTextFieldEx str mEditAction = liftS $ \path vidC -> (textField (ViewId $ path ++ [vidC]) str mEditAction, vidC + 1)

mkPasswordField str = mkPasswordFieldEx str Nothing

--mkPasswordFieldAct str act = mkPasswordFieldEx str $ Just act

mkPasswordFieldEx str mEditAction = liftS $ \path vidC -> (passwordField (ViewId $ path ++ [vidC]) str mEditAction , vidC + 1)

mkTextArea str = liftS $ \path vidC -> (textArea (ViewId $ path ++ [vidC]) str, vidC + 1)

widgetGetViewRef (Widget (ViewId vid) _ _ _) = ViewIdRef vid
                  

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





mkViewEdit i f = 
  ViewEdit i $ \(WebView vi si i lv v) -> WebView vi si i lv $ applyIfCorrectType f v


applyIfCorrectType :: (Typeable y, Typeable x) => (y -> y) -> x -> x
applyIfCorrectType f x = case cast f of 
                           Just fx -> fx x
                           Nothing -> x

-- the view matching on load can be done explicitly, following structure and checking ids, or
-- maybe automatically, based on id. Maybe extra state can be in a separate data structure even,
-- like in Proxima
loadView :: WebView -> WebViewM WebView
loadView (WebView _ si i mkView oldView') =
 do { WebViewState user db viewMap path viewIdCounter <- get
    ; let newViewId = ViewId $ path ++ [viewIdCounter]                             
    ; put $ WebViewState user db viewMap (path++[viewIdCounter]) viewIdCounter
    ; let oldView = case lookupOldView newViewId viewMap of
                      Just oldView -> oldView
                      Nothing      -> oldView' -- this will be initial
                      
    ; view <- mkView newViewId oldView -- path is one level deeper for children created here 
    ; put $ WebViewState user db viewMap path $ viewIdCounter + 1 -- restore old path
    ; return $ WebView newViewId si i mkView view    
    }
mkWebView :: (Presentable v, Storeable v, Initial v, Show v, Eq v, Data v) =>
             (ViewId -> v -> WebViewM v) ->
             WebViewM WebView
mkWebView mkView  =
 do { let initialWebView = WebView noViewId noId noId mkView initial
    ; webView <- loadView initialWebView
    ; return webView
    } 

{-

Everything seems to work:
not updating the unchanged controls and keeping id's unique causes edit events survive update
They do seem to lose focus though, but since we know what was edited, we can easily restore the focus after
the update
-}


-- textfields are in forms, that causes registering text field updates on pressing enter
-- (or Done) on the iPhone.

presentTextField :: Text -> Html
presentTextField (Text (Id i) TextArea str _) = 
   form![ thestyle "display: inline; width: 500px;"
        , strAttr "onSubmit" $ "textFieldChanged('"++show i++"'); return false"] $
     textarea ! [ identifier (show i)
                , thestyle "width: 100%; height: 100%;"
                --, strAttr "onChange" $ "textFieldChanged('"++show i++"')"
                , strAttr "onFocus" $ "elementGotFocus('"++show i++"')"
                , strAttr "onBlur" $ "textFieldChanged('"++show i++"')"
                ] << stringToHtml str
presentTextField (Text (Id i) textType str mEditAction) = 
  let inputField = case textType of TextField -> textfield ""
                                    PasswordField -> password ""
                                    
  in form![ thestyle "display: inline"
          , strAttr "onSubmit" $ "textFieldChanged('"++show i++"');" ++
                                 (case mEditAction of
                                    Nothing -> []
                                    Just _  -> "queueCommand('SubmitC "++show i++"'); ")++
                                 "return false"] $
       inputField ! [ identifier (show i), strAttr "value" str, width "100%"
                    --, strAttr "onChange" $ "textFieldChanged('"++show i++"')"
                    , strAttr "onFocus" $ "elementGotFocus('"++show i++"')"
                    , strAttr "onBlur" $ "textFieldChanged('"++show i++"')" ]

-- seems like this one could be in Present
presentButton :: Button -> Html
presentButton (Button (Id i) txt enabled _) = 
   primHtml $ "<button id=\""++ show i++"\" "++ (if enabled then "" else "disabled ") ++
                            "onclick=\"queueCommand('ButtonC "++show i++"')\" "++
                            "onfocus=\"elementGotFocus('"++show i++"')\">"++txt++"</button>"
-- TODO: text should be escaped


-- Edit actions are a bit different, since they do not have a widget presentation.
-- TODO: maybe combine edit actions with buttons, so they use the same command structure
-- a descriptive text field can be added to the action, to let the server be able to show
-- which button was pressed. However, it is not sure if this works okay with restoring id's
-- though it probably works out, as the ea id is the only one needing restoration.
withEditAction (EditAction (Id i) _) elt = 
  thespan![ identifier $ show i 
          , strAttr "onClick" $ "queueCommand('PerformEditActionC "++show i++"')"] << elt

withEditActionAttr (EditAction (Id i) _) = 
  strAttr "onClick" $ "queueCommand('PerformEditActionC "++show i++"')"

presentRadioBox :: RadioView -> Html
presentRadioBox (RadioView (Id id) items selectedIx enabled) = thespan << 
  [ radio (show id) (show i) ! ( [ identifier eltId 
                          , strAttr "onChange" ("queueCommand('SetC "++show id++" %22"++show i++"%22')") 
                          , strAttr "onFocus" ("elementGotFocus('"++eltId++"')")
                          ]
                          ++ (if enabled && i == selectedIx then [strAttr "checked" ""] else []) 
                          ++ (if not enabled then [strAttr "disabled" ""] else [])) 
                          +++ item +++ br 
  | (i, item) <- zip [0..] items 
  , let eltId = "radio"++show id++"button"++show i ] -- these must be unique for setting focus

instance Presentable WebView where
  present (WebView _ (Id stubId) _ _ _) = mkSpan (show stubId) << "ViewStub"
  
instance Presentable (Widget x) where
  present (Widget _ (Id stubId) _ _) = mkSpan (show stubId) << "WidgetStub"


-- todo button text and radio text needs to go into view
instance Presentable AnyWidget where                          
  present (RadioViewWidget rv) = presentRadioBox rv 
  present (TextWidget es) = presentTextField es 
  present (ButtonWidget b) = presentButton b 
  present (EditActionWidget b) = noHtml



-- Login -----------------------------------------------------------------------  

data LoginView = LoginView (Widget Text) (Widget Text) (Widget Button) 
  deriving (Eq, Show, Typeable, Data)

instance Initial LoginView where initial = LoginView initial initial initial

mkLoginView = mkWebView $
  \vid (LoginView name password b) ->
   fixView $ \fixedAuthenticate ->
   do { nameT <- mkTextField (getStrVal name)
      ; passwordT <- mkPasswordField (getStrVal password)
--      ; nameT <- mkTextFieldAct (getStrVal name) fixedAuthenticate 
--      ; passwordT <- mkPasswordFieldAct (getStrVal password) fixedAuthenticate 
      ; let authenticate = AuthenticateEdit (widgetGetViewRef nameT) (widgetGetViewRef passwordT)
      
      ; loginB <- mkButton "Login" True authenticate                   
      ; return $ (LoginView nameT passwordT loginB, authenticate)
      }

instance Storeable LoginView where save _ = id
                                   
instance Presentable LoginView where
  present (LoginView name password loginbutton) = 
    boxed $ simpleTable [] [] [ [ stringToHtml "Login:", present name]
                              , [ stringToHtml "Password:", present password]
                              , [ present loginbutton ]
                              ]
            


-- Logout ----------------------------------------------------------------------  

data LogoutView = LogoutView (Widget Button) deriving (Eq, Show, Typeable, Data)

instance Initial LogoutView where initial = LogoutView initial

mkLogoutView = mkWebView $
  \vid _ -> 
   do { (Just (l,_)) <- getUser
      ; logoutB <- mkButton ("Logout " ++  l) True LogoutEdit
      ; return $ LogoutView logoutB
      }
instance Storeable LogoutView where save _ = id
                                   
instance Presentable LogoutView where
  present (LogoutView logoutbutton) = 
    present logoutbutton
            


-- LinkView ---------------------------------------------------------------------  

-- This is a separate view for editActions. Putting edit actions inside a view that is changed
-- may cause press events to get lost. This indirection solves the problem.
data LinkView = LinkView String EditAction deriving (Eq, Show, Typeable, Data)

instance Initial LinkView where initial = LinkView initial initial

mkLinkView linkText action = mkWebView $
  \vid _ ->
   do { editAction <- mkEditAction action
      ; return $ LinkView linkText editAction
      }
   
instance Storeable LinkView where save _ = id

instance Presentable LinkView where
  present (LinkView linkText editAction) = withEditAction editAction $ stringToHtml linkText



--------------- Utils ---------------

fixView :: (args -> WebViewM (v, args)) -> WebViewM v
fixView mkv = 
  do { (v, _) <- mfix (\(~(_,args)) -> mkv args)
     ; return v
     }
