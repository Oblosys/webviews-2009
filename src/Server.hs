{-# OPTIONS -fglasgow-exts #-}
module Server where

import Happstack.Server
import Happstack.Helpers hiding (HtmlString)
import System.IO
import Data.List
import Data.IORef
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import System.Posix.Time
import System.Posix.Types
import Text.Html
import Control.Exception
import qualified Data.ByteString.Char8 as Bytestring
import qualified Codec.Binary.Base64.String as Base64

import Types
import Generics
import Database
import WebViews
import WebViewLib
import Incrementality
import HtmlLib

webViewsPort = 8085


{-
Happstack notes
Server error: Prelude.last: empty list
is the error you get when fileServe cannot find a file

check caching in case of problems

ServerPart is basically a Reader monad for requests

The Ok part of the WebT monad contains a function out that is applied to the responses before
sending them to the client. If the result is of type Response, set/addHeader can be fmapped to
the monad, but it will only do something if the header is not set in the out part of Ok.

Header modifications must therefore be applied to out rather than be fmapped to the monad.
-}
type GlobalState = (Database, Sessions, SessionCounter)

initGlobalState = (theDatabase, IntMap.empty, 0)

type GlobalStateRef = IORef GlobalState

type ServerInstanceId = EpochTime

type SessionId = Int

type SessionCounter = Int

type Sessions = IntMap (User, WebView, Maybe EditCommand) 

type SessionState = (SessionId, User, Database, WebView, Maybe EditCommand)

type SessionStateRef = IORef SessionState

getRootView :: SessionStateRef -> IO WebView
getRootView sessionStateRef =
 do { (_, _, _, rootView, _) <- readIORef sessionStateRef
    ; return rootView
    }
 
setRootView :: SessionStateRef -> WebView -> IO ()
setRootView sessionStateRef rootView =
 do { (sessionId, user, db, _, pendingEdit) <- readIORef sessionStateRef
    ; writeIORef sessionStateRef (sessionId, user, db, rootView, pendingEdit)
    }
 
server =
 do { serverSessionId <- epochTime
    ; globalStateRef <- newIORef initGlobalState
    
    ; dbStr <-
       do { fh <- openFile "Database.txt" ReadMode
          ; dbStr <- hGetContents fh 
          ; seq (length dbStr) $ return ()
          ; hClose fh
          ; return dbStr
          } `Control.Exception.catch` \(exc :: SomeException) ->
       do { putStrLn $ "On opening Database.txt:\n"
          ; putStrLn $ "Exception "++ show exc
          ; putStrLn $ "\nUsing default database."
          ; return ""
          }
       
    ; case safeRead dbStr of
        Just db -> modifyIORef globalStateRef $
                     \(_, sessions, sessionCounter) -> (db, sessions, sessionCounter)
        Nothing -> return ()
    ; simpleHTTP (Conf webViewsPort Nothing) $ 
        msum (handlers serverSessionId globalStateRef)
    }

{-
handle:
http://<server url>/                    response: <executable dir>/WebViews.html
http://<server url>/favicon.ico         response: <executable dir>/favicon.icome>
http://<server url>/handle?commands=Commands ..                    
                                        response: from handleCommands
-}


handlers :: ServerInstanceId -> GlobalStateRef -> [ServerPart Response]
handlers serverSessionId globalStateRef = 
  [ exactdir "/" $
     do { liftIO $ putStrLn "Root requested"
        ; fileServe ["scr/WebViews.html"] "."
        }
  , dir "favicon.ico" $
      methodSP GET $ fileServe ["favicon.ico"] "."
  , dir "scr" $
      fileServe [] "scr"  
  , dir "img" $
      fileServe [] "img"  
  , debugFilter $ 
      dir "handle" $ 
        withData (\cmds -> methodSP GET $ session serverSessionId globalStateRef cmds)
  ]
{- TODO: why does exactdir "/handle" not work?
   TODO: fix syntax error in command

-}

type SessionCookie = (String, String)

session :: ServerInstanceId -> GlobalStateRef -> Commands -> ServerPart Response
session serverInstanceId globalStateRef cmds =
 do { mCookieSessionId <- parseCookieSessionId serverInstanceId
      
--        ; lputStrLn $ show rq
        ; sessionId <- case mCookieSessionId of 
            Nothing  -> createNewSessionState globalStateRef serverInstanceId
            Just key -> do { lputStrLn $ "Existing session "++show key
                           ; return key
                           }
             
        ; sessionStateRef <- retrieveSessionState globalStateRef sessionId 
                                  
        ; responseHtml <- sessionHandler sessionStateRef cmds              
        
        ; storeSessionState globalStateRef sessionId sessionStateRef
        
        ; ok $ toResponse $ responseHtml
        }
 
parseCookieSessionId :: ServerInstanceId -> ServerPart (Maybe SessionId)
parseCookieSessionId serverInstanceId = 
 do { rq <- askRq
             
    ; let cookieMap = rqCookies rq
    ; let mCookieSessionId = case lookup "webviews" cookieMap of
                      Nothing -> Nothing -- * no webviews cookie on the client
                      Just c  -> case safeRead (cookieValue c) of
                                   Nothing               -> Nothing -- * ill formed cookie on client
                                   Just (serverTime::EpochTime,key::Int) -> 
                                     if serverTime /= serverInstanceId
                                     then Nothing  -- * cookie from previous WebViews run
                                     else Just key -- * correct cookie for this run
    ; return mCookieSessionId
    } -- TODO: this is nasty, maybe try to use the Happs function
 
mkInitialRootView :: IO WebView
mkInitialRootView = runWebView Nothing theDatabase Map.empty [] 0 $ mkWebView (\_ _ -> return ()) 

-- this creates a WebView with stubid 0 and id 1
-- for now, we count on that in the client
-- TODO: change this to something more robust
-- todo: use different id

createNewSessionState :: GlobalStateRef -> ServerInstanceId -> ServerPart SessionId
createNewSessionState globalStateRef serverInstanceId = 
 do { (database, sessions,sessionCounter) <- liftIO $ readIORef globalStateRef
    ; let sessionId = sessionCounter
    ; lputStrLn $ "New session: "++show sessionId
    ; addCookie 3600 (mkCookie "webviews" $ show (serverInstanceId, sessionId))
    -- cookie lasts for one hour
 
    ; initialRootView <- liftIO $ mkInitialRootView
                         
    ; let newSession = (Nothing, initialRootView, Nothing)
    ; let sessions' = IntMap.insert sessionId newSession sessions
   
    ; liftIO $ writeIORef globalStateRef (database, sessions', sessionCounter + 1)
    
    ; return sessionId
    }
 
retrieveSessionState :: GlobalStateRef -> SessionId -> ServerPart SessionStateRef
retrieveSessionState globalStateRef sessionId =
 do { (database, sessions, sessionCounter) <- liftIO $ readIORef globalStateRef
    ; lputStrLn $ "\n\nNumber of active sessions: " ++ show sessionCounter                                          
    ; sessionState <- case IntMap.lookup sessionId sessions of -- in monad to show errors (which are not caught :-( )
                             Nothing                      -> do { lputStrLn "\n\n\n\nInternal error: Session not found\n\n\n\n\n"
                                                                ; error "Internal error: Session not found"
                                                                }
                             Just (user, rootView, pendingEdit) -> 
                               return (sessionId, user, database, rootView, pendingEdit)
    ; liftIO $ newIORef sessionState
    }      
 
storeSessionState :: GlobalStateRef -> SessionId -> SessionStateRef -> ServerPart ()
storeSessionState globalStateRef sessionId sessionStateRef =
 do { (_, sessions, sessionCounter) <- liftIO $ readIORef globalStateRef
    ; (_, user', database', rootView', pendingEdit') <- liftIO $ readIORef sessionStateRef
    ; let sessions' = IntMap.insert sessionId (user', rootView', pendingEdit') sessions                                          
    ; liftIO $ writeIORef globalStateRef (database', sessions', sessionCounter)
    }
 
sessionHandler :: SessionStateRef -> Commands -> ServerPart Html
sessionHandler sessionStateRef cmds = liftIO $  
 do { putStrLn $ "Received commands" ++ show cmds
    
    ; (_, _, db, oldRootView', _) <- readIORef sessionStateRef
    
    -- TODO: this elem construction is not so nice if Init is part of multiple commands
    ; oldRootView <- if Init `elem` getCommands cmds 
                     then mkInitialRootView 
                     else return oldRootView' 

          
    ; response <- handleCommands sessionStateRef cmds
    -- handleCommands modifies the state              
                  
    ; responseHtml <- case response of
        ViewUpdate ->
         do { rootViewWithoutIds <- getRootView sessionStateRef
              -- this is the modified rootView                      
                                    
            -- save the database if there was a change
            ; (_, _, db', _, _) <- readIORef sessionStateRef
            ; if db /= db' then 
               do { fh <- openFile "Database.txt" WriteMode
                  ; hPutStr fh $ show db'
                  ; hClose fh
                  }
               else return ()
            
            ; let (rootView, _) = assignIds (clearIds rootViewWithoutIds, oldRootView) 
            -- this way, all new id's are unique, also with respect to old view                                    
            ; setRootView sessionStateRef rootView
              
              
            ; (responseHtml, rootView') <- mkIncrementalUpdates oldRootView rootView
            -- rootView' has different id's (the ones that were not updated and hence are
            -- restored to their previous values)
                                           
            --; putStrLn $ "View tree:\n" ++ drawWebNodes (WebViewNode rootView) 
            --; putStrLn $ "rootView:\n" ++ show rootView'
            ; setRootView sessionStateRef rootView'
            --; putStrLn $ "database:\n" ++ show db
            ; putStrLn $ "\n\n\nresponse = \n" ++ show responseHtml
            ; putStrLn $ "View tree':\n" ++ drawWebNodes (WebViewNode rootView') 
            --; putStrLn $ "Sending response sent to client: " ++
            --              take 10 responseHTML ++ "..."
            ; seq (length (show responseHtml)) $ return ()
            ; return responseHtml
            }
        Alert str -> 
          return $ thediv ! [identifier "updates"] <<
                   (thediv![ strAttr "op" "alert"
                           , strAttr "text" str
                           ] << noHtml) 
        Confirm str  -> 
          return $ thediv ! [identifier "updates"] <<
                    (thediv![ strAttr "op" "confirm"
                            , strAttr "text" str
                            ] << noHtml) 
    
    ; return responseHtml
    } `Control.Exception.catch` \(exc :: SomeException) ->
       do { let exceptionTxt = 
                  "\n\n\n\n###########################################\n\n\n" ++
                  "Exception: " ++ show exc ++ "\n\n\n" ++
                  "###########################################" 
          
          ; putStrLn exceptionTxt
          ; return $ thediv ! [identifier "updates"] <<
                       updateReplaceHtml "root" 
                        (mkDiv "root" $ map (p . stringToHtml) $ lines exceptionTxt)
          }
 where evaluateDbAndRootView sessionStateRef =
        do { dbRootView <- liftIO $ readIORef sessionStateRef
           ; seq (length $ show dbRootView) $ return ()
           }

instance FromData Commands where
  fromData = liftM readCommand (look "commands")

readCommand s = case safeRead s of
                  Just cmds -> cmds
                  Nothing   -> SyntaxError s

 
data ServerResponse = ViewUpdate | Alert String | Confirm String deriving (Show, Eq)

-- handle each command in commands and send the updates back
handleCommands sessionStateRef (SyntaxError cmdStr) =
  error $ "Syntax error in commands from client: "++cmdStr 
handleCommands sessionStateRef (Commands [command]) = handleCommand sessionStateRef command
handleCommands sessionStateRef (Commands commands) = 
 do { responses <- mapM (handleCommand sessionStateRef) commands
    ;  case dropWhile (== ViewUpdate) responses of
         []         -> return ViewUpdate
         [response] -> return response
         _          -> error "Non View update commmand followed by other commands"
                    -- probably okay if they are all ViewUpdates
    } -- TODO: think of a way to handle multiple commands and dialogs etc.
      --       make sure that id's are not generated between commands


handleCommand :: SessionStateRef -> Command -> IO ServerResponse
handleCommand sessionStateRef Init =
 do { putStrLn "Init"
    ; (sessionId, user, db, oldRootView, _) <- readIORef sessionStateRef
    ; rootView <- liftIO $ mkRootView user db sessionId (mkViewMap oldRootView)
    ; setRootView sessionStateRef rootView
     
    ; return ViewUpdate
    }
handleCommand sessionStateRef Refresh =
 do { putStrLn "Refresh"
    ; reloadRootView sessionStateRef
    ; return ViewUpdate
    }
handleCommand sessionStateRef Test =
 do { (_, user, db, rootView, _) <- readIORef sessionStateRef
    ; return ViewUpdate
    }
handleCommand sessionStateRef (SetC viewId value) =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef      
    ; putStrLn $ show viewId ++ " value is " ++ show value

    ; let rootView' = replace (Map.fromList [(viewId, value)]) (assignIds rootView)
    --; putStrLn $ "Updated rootView:\n" ++ show rootView'
    ; let db' = save rootView' db
      -- TODO: check if mkViewMap has correct arg
    -- TODO: instead of updating all, just update the one that was changed
    ; writeIORef sessionStateRef (sessionId, user, db', rootView', pendingEdit)
    ; reloadRootView sessionStateRef
    ; return ViewUpdate
    }
handleCommand sessionStateRef (ButtonC viewId) =
 do { (_, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; let Button _ txt _ act = getButtonByViewId viewId rootView
    ; putStrLn $ "Button #" ++ show viewId ++ ":" ++ txt ++ " was clicked"

    ; response <- performEditCommand sessionStateRef act
          
    ; return response
    }
handleCommand sessionStateRef (SubmitC viewId) =
 do { (_, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; let Text _ _ txt mAct = getTextByViewId viewId rootView
    ; putStrLn $ "Text #" ++ show viewId ++ ":" ++ txt ++ " was submitted"

    ; response <- case mAct of 
        Nothing  -> error "Internal error: text field with submission action has no associated action."
        Just act -> performEditCommand sessionStateRef act
          
    ; return response
    }
handleCommand sessionStateRef (PerformEditActionC viewId) =
 do { (_, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; let EditAction _ act = getEditActionByViewId viewId rootView
    ; putStrLn $ "EditAction with ViewId "++show viewId ++ " was executed"

    ; response <- performEditCommand sessionStateRef act
          
    ; return response
    }
handleCommand sessionStateRef ConfirmDialogOk =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; writeIORef sessionStateRef (sessionId, user, db, rootView, Nothing) -- clear it, also in case of error
    ; response <- case pendingEdit of
                    Nothing -> error "ConfirmDialogOk event without active dialog"
                    Just ec -> performEditCommand sessionStateRef ec
    ; return response
    }
 
reloadRootView :: SessionStateRef -> IO ()
reloadRootView sessionStateRef =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; rootView' <- evalStateT (loadView rootView) (WebViewState user db (mkViewMap rootView) [] 0)
 -- TODO this 0 does not seem right BUG
    ; writeIORef sessionStateRef (sessionId, user, db, rootView', pendingEdit)
    } 
 
performEditCommand sessionStateRef command =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; case command of  
            AlertEdit str -> return $ Alert str
            ConfirmEdit str ec -> 
             do { writeIORef sessionStateRef (sessionId, user, db, rootView, Just ec)
                ; return $ Confirm str
                }
            AuthenticateEdit userViewId passwordViewId -> authenticate sessionStateRef userViewId passwordViewId
            LogoutEdit -> logout sessionStateRef
            _ ->
             do { (db', rootView') <-
                  case command of
                    DocEdit docedit -> 
                      let db' = docedit db
                      in  return (db', rootView)
                             
                    ViewEdit i viewedit -> 
                      let wv = getWebViewById i rootView
                          wv' = viewedit wv
                          rootView' = replaceWebViewById i wv' rootView 
                                       -- TODO: check if mkViewMap has correct arg
                                       --       make function for loading rootView
                      in  return (save rootView' db, rootView')
                
                ; writeIORef sessionStateRef (sessionId, user, db', rootView', pendingEdit)
                ; reloadRootView sessionStateRef
                --; putStrLn $ "\n\n\n\view before edit:" ++ show rootView
                --; putStrLn $ "\nview after edit:" ++ show rootView'
                ; return ViewUpdate
                }

    }


-- TODO id's may not be right. what if views changed and the fields get assigned different id's
-- than when button was created?
authenticate sessionStateRef userEStringViewId passwordEStringViewId =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; let mUserName = getTextByViewIdRef userEStringViewId rootView
          mEnteredPassword = getTextByViewIdRef passwordEStringViewId rootView
    ; case (mUserName, mEnteredPassword) of
        (Just userName, Just enteredPassword) ->
           case Map.lookup userName users of
             Just (password, fullName) -> if password == enteredPassword  
                                      then 
                                       do { putStrLn $ "User "++userName++" authenticated"
                                          ; writeIORef sessionStateRef 
                                              (sessionId, Just (userName, fullName)
                                              , db, rootView, pendingEdit)
                                          ; reloadRootView sessionStateRef
                                          ; return ViewUpdate
                                          }
                                      else
                                       do { putStrLn $ "User "++userName++" entered a wrong password"
                                          ; return $ Alert "Incorect password"
                                          }
             Nothing -> do { putStrLn $ "User "++userName++" entered a wrong password"
                           ; return $ Alert $ "Unknown username: "++userName
                           }
        _ -> error $ "Internal error: at least one referenced ViewId not in rootView" ++
                     show [userEStringViewId, passwordEStringViewId]
    }
logout sessionStateRef =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; writeIORef sessionStateRef (sessionId, Nothing, db, rootView, pendingEdit)
    ; reloadRootView sessionStateRef
    ; return ViewUpdate
    }  


-- Utils

lputStr :: MonadIO m => String -> m ()
lputStr = liftIO . putStr

lputStrLn :: MonadIO m => String -> m ()
lputStrLn = liftIO . putStrLn

safeRead s = case reads s of
               [(x, "")] -> Just x
               _         -> Nothing


