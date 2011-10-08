module Server where

import Happstack.Server
import System.IO
import Data.List
import Data.Maybe
import Data.IORef
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import System.Time (getClockTime)
import System.Posix.Time
import System.Posix.Types
import Text.Html
import Control.Exception
import qualified Data.ByteString.Char8 as Bytestring
import qualified Codec.Binary.Base64.String as Base64

import Types
import Generics
import WebViewPrim
import Incrementality
import HtmlLib

webViewsPort = 8090


{-
TODO: why are there so many theDatabase refs? when is this thing initialized and when is it read from disk?
see if passing down mkRootView, dbFilename, theDatabase and users can be improved (maybe in state?)
             

maybe use type class?

check dummy args (witnesses?) in Generics for replace and getTextByViewIdRef

bug: webview fields that are not presented still show up in interface

todo: initials are annoying, figure out TH stuff and write down what they are for

viewEdit is completetely untyped!!

two thestyle's together don't work, so thestyle .. and later colorAttr should be unified in one thestyle 


Check lenses
Check relation to google toolkit. Maybe we can use stuff from it?



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

{-

Buttons that are not found are ignored
ConfirmDialog is ignored if dialog is not here
TODO: make confirmDialog more robust


-}
type GlobalState db = (db, Sessions db, SessionCounter)

initGlobalState theDatabase = (theDatabase, IntMap.empty, 0)

type GlobalStateRef db = IORef (GlobalState db)

type ServerInstanceId = EpochTime

type SessionCounter = Int

type Sessions db = IntMap (User, WebView db, Maybe (EditCommand db))

type SessionStateRef db = IORef (SessionState db)

getRootView :: SessionStateRef db -> IO (WebView db)
getRootView sessionStateRef =
 do { (_, _, _, rootView, _) <- readIORef sessionStateRef
    ; return rootView
    }
 
setRootView :: SessionStateRef db -> WebView db -> IO ()
setRootView sessionStateRef rootView =
 do { (sessionId, user, db, _, pendingEdit) <- readIORef sessionStateRef
    ; writeIORef sessionStateRef (sessionId, user, db, rootView, pendingEdit)
    }
 
server :: (Data db, Typeable db, Show db, Read db, Eq db) =>
          [ (String, Int -> WebViewM db (WebView db))] -> String -> db -> Map String (String, String) -> IO ()
server rootViews dbFilename theDatabase users =
 do { hSetBuffering stdout NoBuffering -- necessary to run server in Eclipse
    ; time <- getClockTime
    ; putStrLn $ "\n\n### Started WebViews server (port "++show webViewsPort++"): "++show time ++"\n"
    ; serverSessionId <- epochTime
    ; globalStateRef <- newIORef $ initGlobalState theDatabase
    
    ; dbStr <-
       do { fh <- openFile dbFilename ReadMode
          ; dbStr <- hGetContents fh 
          ; seq (length dbStr) $ return ()
          ; hClose fh
          ; return dbStr
          } `Control.Exception.catch` \exc ->
       do { putStrLn $ "On opening "++dbFilename++":\n"
          ; putStrLn $ "Exception "++ show (exc :: SomeException)
          ; putStrLn $ "\nUsing default database."
          ; return ""
          }
       
    ; case safeRead dbStr of
        Just db -> modifyIORef globalStateRef $
                     \(_, sessions, sessionCounter) -> (db, sessions, sessionCounter)
        Nothing -> return ()
    ; simpleHTTP nullConf { port = webViewsPort, logAccess = Nothing {-Just logWebViewAccess-} } $ 
        msum (handlers rootViews dbFilename theDatabase users serverSessionId globalStateRef)
    }
{-
handle:
http://<server url>/favicon.ico         response: <executable dir>/favicon.icome>
http://<server url>/handle?commands=Commands ..                    
                                        response: from handleCommands
http://<server url>/                    response: <executable dir>/WebViews.html
-}

-- todo: maybe we can use this for debugging
logWebViewAccess :: String -> String -> t -> String -> Int -> Integer -> String -> String -> IO ()
logWebViewAccess clientIP b _ c d e f g =
 do { putStrLn $ show clientIP ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ " " ++ show g ++ " " ++ show g
    }


instance FromData Commands where
  fromData = liftM readCommand (look "commands")

readCommand s = case safeRead s of
                  Just cmds -> cmds
                  Nothing   -> SyntaxError s

instance FromData Int where
  fromData = liftM readInt (look "requestId")

readInt s = fromMaybe (-1) (safeRead s)

handlers :: (Data db, Show db, Eq db) => [ (String, Int -> WebViewM db (WebView db))] -> String -> db -> Map String (String, String) -> ServerInstanceId -> GlobalStateRef db -> [ServerPart Response]
handlers rootViews dbFilename theDatabase users serverSessionId globalStateRef = 
  [ dir "favicon.ico" $ serveDirectory DisableBrowsing [] "favicon.ico"
  , dir "scr" $ serveDirectory DisableBrowsing [] "scr"  
  , dir "img" $ serveDirectory DisableBrowsing [] "img"  
  , dir "handle" $ 
      withData (\cmds -> do { requestIdData <- getData
                            ; requestId <- case requestIdData of
                                            Right i |  i/=(-1)  -> return i
                                            Right i | otherwise -> do { liftIO $ putStrLn "Unreadable requestId";    mzero }
                                            Left err            -> do { liftIO $ putStrLn "No requestId in request"; mzero }
                                
                            ; liftIO $ putStrLn $ "RequestId: "++show (requestId :: Int)
                            ; methodSP GET $ session rootViews dbFilename theDatabase users serverSessionId globalStateRef requestId cmds
                            })
  , uriRest $ \rootViewName -> anyPath $ serveMainPage rootViewName  -- http://webviews.com/rootViewName
             -- we don't need to do anything with the rootViewName here, since the client takes the view name from the
             -- browser url and passes it along with the Init event
  , serveMainPage ""
  ] 
 where serveMainPage rootViewName =
        do { liftIO $ putStrLn $ "Root requested "++rootViewName
           ; serveDirectory DisableBrowsing [] "scr/WebViews.html"
           } 
 -- TODO handle this differently, so / after rootViewName can be handled (now a problem since it causes rootViewName/handle requests instead of just handle)
 -- maybe do   msum[ path $ \rootViewName -> handlers rootViewName, handlers ""] This allows a more elegant way to extract
 -- the rootViewName directly from the path, without having the client take it from document.location.href
 -- Should check what happens when there is no / after the rootViewName though, as this may complicate things (or maybe
 -- just disallow this). Also the script should be fixed, so scr and img links are absolute (e.g. "/scr/" instead of "src/..") 
    
{-
This stuff may not hold for HappStack 6
 TODO: why does exactdir "/handle" not work?
   TODO: fix syntax error in command

this get command works (there should not be an unescaped space between Commands and [Init]
GET /handle?commands=Commands[Init] HTTP/1.1

Response:
HTTP/1.1 200 OK
Connection: Keep-Alive
Content-Length: 32597
Content-Type: text/html
Date: Sat, 16 May 2009 18:22:28 GMT
Server: Happstack/0.2.1
Set-Cookie: webviews="(1242497513,2)";Max-Age=3600;Path=/;Version="1"

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 FINAL//EN">
...

-}

type SessionCookie = (String, String)

session :: (Data db, Show db, Eq db) => [ (String, Int -> WebViewM db (WebView db))] -> String -> db -> Map String (String, String) -> ServerInstanceId -> GlobalStateRef db -> Int -> Commands -> ServerPart Response
session rootViews dbFilename theDatabase users serverInstanceId globalStateRef requestId cmds =
 do { mCookieSessionId <- parseCookieSessionId serverInstanceId
      
--        ; lputStrLn $ show rq
        ; sessionId <- case mCookieSessionId of 
            Nothing  -> createNewSessionState theDatabase globalStateRef serverInstanceId
            Just key -> do { --lputStrLn $ "Existing session "++show key
                           ; return key
                           }
             
        ; sessionStateRef <- retrieveSessionState globalStateRef sessionId 
                                  
        ; responseHtml <- sessionHandler rootViews dbFilename theDatabase users sessionStateRef requestId cmds              
        
        ; storeSessionState globalStateRef sessionId sessionStateRef
        
        ; liftIO $ putStrLn "Done"
        ; ok $ toResponse $ responseHtml
        }
 
parseCookieSessionId :: ServerInstanceId -> ServerPart (Maybe SessionId)
parseCookieSessionId serverInstanceId = 
 do { rq <- askRq
             
    ; let cookieMap = rqCookies rq
    ; let mCookieSessionId = case lookup "webviews" cookieMap of
                      Nothing -> Nothing -- * no webviews cookie on the client
                      Just c  -> case safeRead (cookieValue c) :: Maybe (EpochTime, Int) of
                                   Nothing               -> Nothing -- * ill formed cookie on client
                                   Just (serverTime, key) -> 
                                     if serverTime /= serverInstanceId
                                     then Nothing  -- * cookie from previous WebViews run
                                     else Just key -- * correct cookie for this run
    ; return mCookieSessionId
    }

mkInitialRootView :: (Data db, Typeable db) => db -> IO (WebView db)
mkInitialRootView theDatabase = runWebView Nothing theDatabase Map.empty [] 0 $ mkWebView (\_ _ -> return ()) 

-- this creates a WebView with stubid 0 and id 1
-- for now, we count on that in the client
-- TODO: change this to something more robust
-- todo: use different id

createNewSessionState :: Data db => db -> GlobalStateRef db -> ServerInstanceId -> ServerPart SessionId
createNewSessionState theDatabase globalStateRef serverInstanceId = 
 do { (database, sessions,sessionCounter) <- liftIO $ readIORef globalStateRef
    ; let sessionId = sessionCounter
    ; lputStrLn $ "New session: "++show sessionId
    ; addCookie Session (mkCookie "webviews" $ show (serverInstanceId, sessionId))
    -- cookie lasts for one hour
 
    ; initialRootView <- liftIO $ mkInitialRootView theDatabase
                         
    ; let newSession = (Nothing, initialRootView, Nothing)
    ; let sessions' = IntMap.insert sessionId newSession sessions
   
    ; liftIO $ writeIORef globalStateRef (database, sessions', sessionCounter + 1)
    
    ; return sessionId
    }
 
retrieveSessionState :: GlobalStateRef db -> SessionId -> ServerPart (SessionStateRef db)
retrieveSessionState globalStateRef sessionId =
 do { (database, sessions, sessionCounter) <- liftIO $ readIORef globalStateRef
    --; lputStrLn $ "\n\nNumber of active sessions: " ++ show sessionCounter                                          
    ; sessionState <- case IntMap.lookup sessionId sessions of -- in monad to show errors (which are not caught :-( )
                             Nothing                      -> do { lputStrLn "\n\n\n\nInternal error: Session not found\n\n\n\n\n"
                                                                ; error "Internal error: Session not found"
                                                                }
                             Just (user, rootView, pendingEdit) -> 
                               return (sessionId, user, database, rootView, pendingEdit)
    ; liftIO $ newIORef sessionState
    }      
 
storeSessionState :: GlobalStateRef db -> SessionId -> SessionStateRef db -> ServerPart ()
storeSessionState globalStateRef sessionId sessionStateRef =
 do { (_, sessions, sessionCounter) <- liftIO $ readIORef globalStateRef
    ; (_, user', database', rootView', pendingEdit') <- liftIO $ readIORef sessionStateRef
    ; let sessions' = IntMap.insert sessionId (user', rootView', pendingEdit') sessions                                          
    ; liftIO $ writeIORef globalStateRef (database', sessions', sessionCounter)
    }
 
sessionHandler :: (Data db, Show db, Eq db) => [ (String, Int -> WebViewM db (WebView db))] -> String -> db -> Map String (String, String) -> SessionStateRef db -> Int -> Commands -> ServerPart Html
sessionHandler rootViews dbFilename theDatabase users sessionStateRef requestId cmds = liftIO $  
 do { --putStrLn $ "Received commands" ++ show cmds
    
    ; (_, _, db, oldRootView', _) <- readIORef sessionStateRef
    
    ; let isInitCommand (Init _) = True
          isInitCommand _        = False
          
    -- TODO: this may cause problems Init is part of multiple commands
    ; oldRootView <- if any isInitCommand $ getCommands cmds 
                     then mkInitialRootView theDatabase 
                     else return oldRootView' 
 
          
    ; response <- handleCommands rootViews users sessionStateRef cmds
    -- handleCommands modifies the state              
                  
    ; responseHtml <- case response of
        ViewUpdate ->
         do { rootViewWithoutIds <- getRootView sessionStateRef
              -- this is the modified rootView                      
                                    
            -- save the database if there was a change
            ; (_, _, db', _, _) <- readIORef sessionStateRef
            ; if db /= db' then 
               do { fh <- openFile dbFilename WriteMode
                  ; hPutStr fh $ show db'
                  ; hClose fh
                  }
               else return ()
            
            ; let (rootView, _) = assignIds (clearIds rootViewWithoutIds, oldRootView) 
            -- this way, all new id's are unique, also with respect to old view                                    
            
            ; putStrLn "seq'ing rootView"
            ; seq (length (show rootView)) $ return ()
            ; putStrLn "seq'ed rootView"
            
            ; setRootView sessionStateRef rootView
              
              
            ; (responseHtml, rootView') <- mkIncrementalUpdates oldRootView rootView
            -- rootView' has different id's (the ones that were not updated and hence are
            -- restored to their previous values)
                                           
            --; putStrLn $ "View tree:\n" ++ drawWebNodes (WebViewNode rootView) 
            --; putStrLn $ "rootView:\n" ++ show rootView'
            ; setRootView sessionStateRef rootView'
            --; putStrLn $ "database:\n" ++ show db
            --; putStrLn $ "\n\n\nresponse = \n" ++ show responseHtml
            --; putStrLn $ "View tree':\n" ++ drawWebNodes (WebViewNode rootView') 
            --; putStrLn $ "Sending response sent to client: " ++
            --              take 10 responseHTML ++ "..."
            ; putStrLn "seq'ing response"
            ; seq (length (show responseHtml)) $ return ()
            ; putStrLn "end session handler"
            ; return $ responseHtml
            }
        Alert str -> 
          return $ [thediv![ strAttr "op" "alert"
                          , strAttr "text" str
                          ] << noHtml] 
        Confirm str  -> 
          return $ [thediv![ strAttr "op" "confirm"
                          , strAttr "text" str
                          ] << noHtml ] 
    
    ; return $ thediv ! [identifier "updates", strAttr "responseId" $ show requestId] <<responseHtml
    } `Control.Exception.catch` \exc ->
       do { let exceptionText = 
                  "\n\n\n\n###########################################\n\n\n" ++
                  "Exception: " ++ show (exc :: SomeException) ++ "\n\n\n" ++
                  "###########################################" 
          
          ; putStrLn exceptionText
          ; return $ thediv ! [identifier "updates", strAttr "responseId" $ show requestId] <<
                      (thediv![ strAttr "op" "exception"
                              , strAttr "text" exceptionText
                              ] << noHtml)                       
          }
 where evaluateDbAndRootView sessionStateRef =
        do { dbRootView <- liftIO $ readIORef sessionStateRef
           ; seq (length $ show dbRootView) $ return ()
           }

 
data ServerResponse = ViewUpdate | Alert String | Confirm String deriving (Show, Eq)

-- handle each command in commands and send the updates back
handleCommands rootViews users sessionStateRef (SyntaxError cmdStr) =
  error $ "Syntax error in commands from client: "++cmdStr 
handleCommands rootViews users sessionStateRef (Commands [command]) = handleCommand rootViews users sessionStateRef command
handleCommands rootViews users sessionStateRef (Commands commands) = 
 do { responses <- mapM (handleCommand rootViews users sessionStateRef) commands
    ;  case dropWhile (== ViewUpdate) responses of
         []         -> return ViewUpdate
         [response] -> return response
         _          -> error "Non View update commmand followed by other commands"
                    -- probably okay if they are all ViewUpdates
    } -- TODO: think of a way to handle multiple commands and dialogs etc.
      --       make sure that id's are not generated between commands


mkRootView ::Data db => [ (String, Int -> WebViewM db (WebView db))] -> String -> User -> db -> Int -> ViewMap db -> IO (WebView db)
mkRootView rootViews rootViewName user db sessionId viewMap =
  fmap assignIds $ runWebView user db viewMap [] 0 $ mkMainView sessionId
 where mkMainView = case lookup rootViewName rootViews of
                      Nothing -> error $ "Unknown view: "++rootViewName
                      Just mkV -> mkV 

handleCommand :: Data db => [ (String, Int -> WebViewM db (WebView db))] -> Map String (String, String) -> SessionStateRef db -> Command -> IO ServerResponse
handleCommand rootViews _ sessionStateRef (Init rootViewName) =
 do { putStrLn $ "Init "++show rootViewName
    ; (sessionId, user, db, oldRootView, _) <- readIORef sessionStateRef
    ; rootView <- liftIO $ mkRootView rootViews rootViewName user db sessionId (mkViewMap oldRootView)
    ; setRootView sessionStateRef rootView
     
    ; return ViewUpdate
    }
handleCommand _ _ sessionStateRef Refresh =
 do { -- putStrLn "Refresh"
    ; reloadRootView sessionStateRef
    ; return ViewUpdate
    }
handleCommand _ _ sessionStateRef Test =
 do { (_, user, db, rootView, _) <- readIORef sessionStateRef
    ; return ViewUpdate
    }
handleCommand _ _ sessionStateRef (SetC viewId value) =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef      
    ; putStrLn $ "Performing: "++show (SetC viewId value)

    --; putStrLn $ "RootView:\n" ++ show rootView ++"\n\n\n\n\n"
    ; let rootView' = replace db{- dummy arg -} (Map.fromList [(viewId, value)]) (assignIds rootView)
    --; putStrLn $ "Updated rootView:\n" ++ show rootView'
    ; let db' = save rootView' db
      -- TODO: check if mkViewMap has correct arg
    -- TODO: instead of updating all, just update the one that was changed
    ; writeIORef sessionStateRef (sessionId, user, db', rootView', pendingEdit)
    ; reloadRootView sessionStateRef
    ; return ViewUpdate
    }
handleCommand _  users sessionStateRef (ButtonC viewId) =
 do { (_, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; case getButtonByViewId viewId rootView of
        Just (Button _ txt _ _ _ act) ->    
         do { putStrLn $ "Button #" ++ show viewId ++ ":" ++ txt ++ " was clicked"

            ; response <- performEditCommand users  sessionStateRef act
          
            ; return response
            }
        Nothing ->
         do { putStrLn $ "Button #"++show viewId++" not present in view"
            ; return ViewUpdate
            }
    }
handleCommand _ users sessionStateRef (SubmitC viewId) =
 do { (_, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; let TextView _ _ txt mAct = getTextByViewId viewId rootView
    ; putStrLn $ "TextView #" ++ show viewId ++ ":" ++ txt ++ " was submitted"

    ; response <- case mAct of 
        Nothing  -> error "Internal error: text field with submission action has no associated action."
        Just act -> performEditCommand users sessionStateRef act
          
    ; return response
    }
handleCommand _ users sessionStateRef (PerformEditActionC viewId args) =
 do { (_, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; let EditAction _ act = getEditActionByViewId viewId rootView
    ; putStrLn $ "EditAction with ViewId "++show viewId ++ " was executed"

    ; response <- performEditCommand users sessionStateRef $ act args
          
    ; return response
    }
handleCommand _ users sessionStateRef ConfirmDialogOk =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; writeIORef sessionStateRef (sessionId, user, db, rootView, Nothing) -- clear it, also in case of error
    ; response <- case pendingEdit of
                    Nothing -> return ViewUpdate -- error "ConfirmDialogOk event without active dialog"
                    Just ec -> performEditCommand users sessionStateRef ec
    ; return response
    }
 
reloadRootView :: Data db => SessionStateRef db -> IO ()
reloadRootView sessionStateRef =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; rootView' <- evalStateT (loadView rootView) (WebViewState user db (mkViewMap rootView) [] 0)
 -- TODO this 0 does not seem right BUG
    ; writeIORef sessionStateRef (sessionId, user, db, rootView', pendingEdit)
    } 
 
performEditCommand users  sessionStateRef command =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; case command of  
            AlertEdit str -> return $ Alert str
            ConfirmEdit str ec -> 
             do { writeIORef sessionStateRef (sessionId, user, db, rootView, Just ec)
                ; return $ Confirm str
                }
            AuthenticateEdit userViewId passwordViewId -> authenticate users  sessionStateRef userViewId passwordViewId
            LogoutEdit -> logout sessionStateRef
            Edit edit -> performEdit sessionStateRef edit
    }
 
performEdit :: Data db => SessionStateRef db -> EditM db () -> IO ServerResponse
performEdit sessionStateRef edit  =
 do { state <- readIORef sessionStateRef
    ; state' <- execStateT edit state
    ; writeIORef sessionStateRef state'
    ; reloadRootView sessionStateRef
    ; return $ ViewUpdate
    }

authenticate users sessionStateRef userEStringViewId passwordEStringViewId =
 do { (sessionId, user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; let userName = getTextByViewIdRef db{-dummy arg-} userEStringViewId rootView
          enteredPassword = getTextByViewIdRef db{-dummy arg-} passwordEStringViewId rootView
    ; case Map.lookup userName users of
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


