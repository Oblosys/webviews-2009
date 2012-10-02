{-# LANGUAGE CPP, DoRec, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module WebViewLib where

import BlazeHtml
import qualified Text.Html as Html
import Data.Generics
import Control.Monad.State
import Types
import Utils
import Generics
import HtmlLib
import WebViewPrim

-- Login -----------------------------------------------------------------------  

data LoginView db = LoginView (Widget (TextView db)) (Widget (TextView db)) (Widget (Button db)) 
  deriving (Eq, Show, Typeable, Data)

instance Initial (LoginView db) where initial = LoginView initial initial initial

instance Data db => MapWebView db (LoginView db) where
  mapWebView (LoginView a b c) = LoginView <$> mapWebView a <*> mapWebView b <*> mapWebView c

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
    boxed $ simpleTable [] [] [ [ "Login:", present name]
                              , [ "Password:", present password]
                              , [ present loginbutton ]
                              ]
            


-- Logout ----------------------------------------------------------------------  

data LogoutView db = LogoutView (Widget (Button db)) deriving (Eq, Show, Typeable, Data)

instance Initial (LogoutView db) where initial = LogoutView initial

instance Data db => MapWebView db (LogoutView db) where
  mapWebView (LogoutView a) = LogoutView <$> mapWebView a

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
data LinkView db = LinkView String (Widget (EditAction db)) deriving (Eq, Show, Typeable, Data)

instance Initial (LinkView db) where initial = LinkView initial initial

instance Data db => MapWebView db (LinkView db) where
  mapWebView (LinkView a b) = LinkView <$> mapWebView a <*> mapWebView b

mkLinkView linkText action = mkWebView $
  \vid _ ->
   do { editAction <- mkEditAction action
      ; return $ LinkView linkText editAction
      }
   
instance Storeable db (LinkView db) where save _ = id

instance Presentable (LinkView db) where
  present (LinkView linkText editAction) = withEditAction editAction $ toHtml linkText



-- TabbedView ---------------------------------------------------------------------  
  
data TabbedView db = TabbedView Int [WebView db] [WebView db] deriving (Eq, Show, Typeable, Data)

instance Initial (TabbedView db) where
  initial = TabbedView 0 initial initial

instance MapWebView db (TabbedView db) where
  mapWebView (TabbedView a b c) = TabbedView <$> mapWebView a <*> mapWebView b <*> mapWebView c

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
    hList [ thespan !* [ theclass "tab"
                       , thestyle ("background-color: "++color)
                       ] $ present selectionView 
          | (i,selectionView) <- zip [0..] selectionViews
          , let color = htmlColor $ if i == selectedTab then Color white else Rgb 200 200 200
          ] +++
    (roundedBoxed (Just $ Color white) $
     concatHtml [ thediv ! attr $ present tabView 
                | (i,tabView) <- zip [0..] tabViews 
                , let attr = thestyle $ "display: " ++ if i == selectedTab 
                                                       then "visible"
                                                       else "none"
                ])

{- version that uses jQuery tabs. Does weird things with font and buttons
instance Presentable TabbedView where
  present (TabbedView _ tabViews) = 
    thediv![theclass "tabbed"] <<
      ((ulist $ concatHtml [li $ anchor![href $ "#"++mkHtmlViewId webView] $ stringToHtml label 
                           | (webView,label)  <- zip tabViews ["een","twee","drie"] ] ) +++
       (concatHtml [ mkDiv (mkHtmlViewId tabView) $ present tabView | tabView <- tabViews ] ))
-}


-- HtmlView ---------------------------------------------------------------------  
--
-- Simple inactive webview that presents its html contents

data HtmlView = HtmlView String deriving (Eq, Show, Typeable, Data)

instance Initial HtmlView where
  initial = HtmlView "HtmlTemplateView not initialized"

instance MapWebView db HtmlView where
  mapWebView (HtmlView a) = HtmlView <$> mapWebView a

mkHtmlView ::  Data db => String -> WebViewM db (WebView db)
mkHtmlView html = mkWebView $
 \vid (HtmlView _) ->
   do { return $ HtmlView html
      }

instance Presentable HtmlView where
  present (HtmlView htmlStr) = primHtml htmlStr

instance Data db => Storeable db HtmlView



-- HtmlTemplateView ---------------------------------------------------------------------  
-- 
-- Non-cached WebView for displaying raw html content read from a file in /htmlTemplates.
-- Placeholders are of the format __placeholderName__.

data HtmlTemplateView = HtmlTemplateView String deriving (Eq, Show, Typeable, Data)

instance Initial HtmlTemplateView where
  initial = HtmlTemplateView "HtmlTemplateView not initialized"
  
instance MapWebView db HtmlTemplateView where
  mapWebView (HtmlTemplateView a) = HtmlTemplateView <$> mapWebView a

mkHtmlTemplateView ::  Data db => String -> [(String,String)] -> WebViewM db (WebView db)
mkHtmlTemplateView path subs = mkWebView $
 \vid (HtmlTemplateView _) ->
   do { templateStr <- liftIO $ readUTFFile $ "htmlTemplates/"++path
      -- TODO: warn for non-existing placeholders
      ; return $ HtmlTemplateView $ substitute subs templateStr
      }

instance Presentable HtmlTemplateView where
  present (HtmlTemplateView htmlStr) = primHtml htmlStr

instance Data db => Storeable db HtmlTemplateView


-- MaybeView ---------------------------------------------------------------------  

data MaybeView db = MaybeView String (Maybe (WebView db)) deriving (Eq, Show, Typeable, Data)


instance Initial (MaybeView db) where
  initial = MaybeView "MaybeView not initialized" Nothing

instance MapWebView db (MaybeView db) where
  mapWebView (MaybeView a b) = MaybeView <$> mapWebView a <*> mapWebView b
 
-- TODO: do we want to offer the vid also to mWebViewM? (which will then have type ViewId -> WebViewM db (Maybe (WebView db)))
mkMaybeView :: Data db => String -> WebViewM db (Maybe (WebView db)) -> WebViewM db (WebView db)
mkMaybeView nothingStr mWebViewM = mkWebView $
 \vid (MaybeView _ _) ->
   do { mWebView <- mWebViewM
      ; return $ MaybeView nothingStr mWebView
      }

instance Presentable (MaybeView db) where
  present (MaybeView nothingStr mWebView) =
    case mWebView of Just webView -> present webView
                     Nothing      -> toHtml nothingStr

instance Data db => Storeable db (MaybeView db)



-- Experimental webviews

-- DialogView ---------------------------------------------------------------------  

-- dialog view that can be added anywhere in the presentation
-- 
-- todo: need to make this phantom typed, so firing the dialog is safer

data DialogView db = DialogView Bool (Widget (EditAction db)) (Maybe (WebView db)) 
    deriving (Eq, Show, Typeable, Data)

instance Data db => Initial (DialogView db) where
  initial = DialogView False initial initial

instance Data db => MapWebView db (DialogView db) where
  mapWebView (DialogView a b c) = DialogView <$> mapWebView a <*> mapWebView b <*> mapWebView c

instance Data db => Storeable db (DialogView db)

viewShowDialog :: forall db . Data db => WebView db -> EditM db ()
viewShowDialog dialogView = viewEdit (getViewId dialogView) $ \(DialogView _ a v :: DialogView db) -> (DialogView True a v)

-- todo add mSearchTerm
mkDialogView :: forall db . Data db => WebViewM db (WebView db) -> WebViewM db (WebView db)
mkDialogView contentsViewM = mkWebView $
  \vid oldView@(DialogView isShowing _ _) ->
    do { cancelAction <- mkEditAction $ Edit $ viewEdit vid $ \(DialogView _ a v :: DialogView db) -> (DialogView False a v)
       ; mContentsView <- 
           if isShowing
           then fmap Just contentsViewM
           else return Nothing
       ; return $ ( DialogView isShowing cancelAction mContentsView
               --   , showAction
                  )
       }

instance Presentable (DialogView db) where
  present (DialogView _ _       Nothing)             = noHtml
  present (DialogView isShowing cancelAction (Just contentsView)) = 
    withEditAction cancelAction $ -- TODO: withEditAction should be onClick
    (div_ ! thestyle "position: absolute; top:0; left:0; width:100%; height: 100%; background-color:black; opacity:0.1; filter:alpha(opacity=40);" $ noHtml) >>
    (div_ ! thestyle "position: absolute; top:0; left:0; width:100%; height: 100%; " $ 
      table !* [cellpadding "0", cellspacing "0", width "100%", height "100%"] $ tr $ 
        sequence_ [ td ! width "50%" $ noHtml
                  , td ! align "center" $
                      div_ !* [onclick "event.stopPropagation();", thestyle "border: 1px solid black; padding: 3px; background-color: lightgrey"] $
                        present contentsView 
                  , td ! width "50%" $ noHtml
                  ]
    )


-- PresentView ---------------------------------------------------------------------  

{- Very experimental webview for providing presentation functions as arguments, instead of declaring instances.

This requires the function to be in the WebView, and hence instance of Data, Typeable, Eq, and Show.

Data and Typeable are handled by creating a Wrapped type with a Data instance that does not descend into its argument (not
sure if this is correct now)

Because the present function may change, we need to test for equality. This can be done by supplying a list of dummy
noHtml arguments with the same length as the list of webview children. Differences in length are caught by comparing the child lists.

Comparing Html is not very efficient, so PresentViews should be used only for small presentations.

The Eq constraint on the presentation is the reason why it has type [Html] -> Html instead of a -> Html, as we cannot
compare the latter with dummy arguments, and would have to compare the Html for the entire PresentView, including its children.

TODO: 
    -- don't use ByteString instead of show and string for comparing Html
    -- check Wrapped Data and Typeable instances
-}
newtype Wrapped = Wrapped ([Html] -> Html)

instance Eq Wrapped where
  (==) = error "no == for Wrapped" -- todo: maybe just false?

instance Show Wrapped where
  show _ = "Wrapped" -- TODO: why do we need show? For seq'ing?

instance Initial Wrapped where
  initial = error "no initial for Wrapped"

instance MapWebView db Wrapped

instance Typeable Wrapped where
  typeOf _ = mkTyConApp (mkTyCon3 "WebViews" "Main" "Wrapped") []
  
instance  Data Wrapped where
  gfoldl k z x@(Wrapped a) = z x --error "gfold not defined for WrappedHtml" -- z WrappedHtml `k` undefined
  gunfold k z c = error "gunfold not defined for WrappedHtml"
     
  toConstr (Wrapped _) =  con_Html
 
  dataTypeOf _ = ty_Html
  
ty_Html = mkDataType "Wrapped" [con_StateT]
con_Html = mkConstr ty_StateT "Wrapped" [] Prefix

data PresentView db = PresentView Wrapped [WebView db] deriving (Show, Typeable, Data)

instance Eq (PresentView db) where
  (PresentView (Wrapped pres1) wvs1) == (PresentView (Wrapped pres2) wvs2) =
    (show $ pres1 (replicate (length wvs1) $ noHtml)) == (show $ pres2 (replicate (length wvs2) $ noHtml)) &&
    wvs1 == wvs2
    -- just compare the html for dummy arguments (since the presentation will never depend on the arguments themselves)

instance Data db => Storeable db (PresentView db)

instance  Initial (PresentView db) where
  initial = PresentView initial initial

instance MapWebView db (PresentView db) where
  mapWebView (PresentView a b) = PresentView <$> mapWebView a <*> mapWebView b

mkPresentView :: Data db => ([Html] -> Html) -> WebViewM db [WebView db] -> WebViewM db (WebView db)
mkPresentView presentList mkSubWebViews = mkWebView $
  \vid oldView@(PresentView _ _) ->
    do { wvs <- mkSubWebViews
       ; return $ PresentView (Wrapped presentList) wvs
       }

instance Presentable (PresentView db) where
  present (PresentView (Wrapped presentList) wvs) = presentList $ map present wvs
