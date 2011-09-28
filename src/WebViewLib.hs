{-# LANGUAGE CPP #-}
module WebViewLib where

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