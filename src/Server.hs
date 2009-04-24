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
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import System.Posix.Time
import System.Posix.Types
import Text.Html
import Control.Exception

import Types
import Generics
import Database
import Views

webViewsPort = 8085

{- authorization demo 
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Codec.Binary.Base64.String as Base64
 -}

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

type Sessions = IntMap (WebView, Maybe EditCommand) 

type SessionState = (Database, WebView, Maybe EditCommand)

type SessionStateRef = IORef SessionState

server =
 do { serverSessionId <- epochTime
    ; globalStateRef <- newIORef initGlobalState
    ; simpleHTTP (Conf webViewsPort Nothing) $ 
        debugFilter $ msum (handlers serverSessionId globalStateRef)
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
         ; fileServe ["WebViews.html"] "."
         }
  , dir "favicon.ico"
        (methodSP GET $ fileServe ["favicon.ico"] ".")
  , dir "img" $
        fileServe [] "img"  
  , dir "handle" $ withData (\cmds -> methodSP GET $ session serverSessionId globalStateRef cmds)
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
 
createNewSessionState globalStateRef serverInstanceId = 
 do { (database, sessions,sessionCounter) <- liftIO $ readIORef globalStateRef
    ; let sessionId = sessionCounter
    ; lputStrLn $ "New session: "++show sessionId
    ; addCookie 3600 (mkCookie "webviews" $ show (serverInstanceId, sessionId))
    -- cookie lasts for one hour
      
    ; let newSession = (mkRootView theDatabase sessionId Map.empty, Nothing)
    ; let sessions' = IntMap.insert sessionId newSession sessions
   
    ; liftIO $ writeIORef globalStateRef (database, sessions', sessionCounter + 1)
    
    ; return sessionId
    }
 
retrieveSessionState globalStateRef sessionId =
 do { (database, sessions, sessionCounter) <- liftIO $ readIORef globalStateRef
    ; lputStrLn $ "\n\nNumber of active sessions: " ++ show sessionCounter                                          
    ; sessionState <- case IntMap.lookup sessionId sessions of -- in monad to show errors (which are not caught :-( )
                             Nothing                      -> do { lputStrLn "\n\n\n\nInternal error: Session not found\n\n\n\n\n"
                                                                ; error "Internal error: Session not found"
                                                                }
                             Just (rootView, pendingEdit) -> return (database, rootView, pendingEdit)
    ; liftIO $ newIORef sessionState
    }                        

storeSessionState globalStateRef sessionId sessionStateRef =
 do { (_, sessions, sessionCounter) <- liftIO $ readIORef globalStateRef
    ; (database', rootView', pendingEdit') <- liftIO $ readIORef sessionStateRef
    ; let sessions' = IntMap.insert sessionId (rootView', pendingEdit') sessions                                          
    ; liftIO $ writeIORef globalStateRef (database', sessions', sessionCounter)
    }
 
sessionHandler sessionStateRef cmds = liftIO $  
 do { putStrLn $ "Received data" ++ show cmds

    ; response <- handleCommands sessionStateRef cmds
    ; responseHtml <- case response of
        ViewUpdate ->
         do { (db, rootView, _) <- readIORef sessionStateRef
            ; let responseHtml = thediv ! [identifier "updates"] <<
                             updateReplaceHtml "root" 
                               (mkDiv "root" $ present $ assignIds rootView)
            ; putStrLn $ "\n\n\n\ncmds = "++show cmds
            --; putStrLn $ "rootView:\n" ++ show (assignIds rootView)
            --; putStrLn $ "database:\n" ++ show db
            --; putStrLn $ "\n\n\nresponse = \n" ++ show responseHtml
            --; putStrLn $ "Sending response sent to client: " ++
            --              take 10 responseHTML ++ "..."
            --; modifyResponseW noCache $
            ; seq (length (show responseHtml)) $ return ()
            ; return responseHtml
            }
        Alert str -> 
          return $ thediv ! [identifier "updates"] <<
                     (thediv![ strAttr "op" "alert"
                             , strAttr "text" str
                             ] << "") 
        Confirm str  -> 
          return $ thediv ! [identifier "updates"] <<
                     (thediv![ strAttr "op" "confirm"
                             , strAttr "text" str
                             ] << "") 
        
    ; return responseHtml
    } `Control.Exception.catch` \(exc :: SomeException) ->
       do { let exceptionTxt = 
                  "\n\n\n\n###########################################\n\n\n" ++
                  "Exception: " ++ show exc ++ "\n\n\n" ++
                  "###########################################" 
                  -- TODO: some exceptions cause program to hang when printed!
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

 
-- handle each command in commands and send the updates back
handleCommands sessionStateRef (SyntaxError cmdStr) =
  error $ "Syntax error in commands from client: "++cmdStr 
handleCommands sessionStateRef (Commands [command]) = handleCommand sessionStateRef command
handleCommands sessionStateRef (Commands commands) = 
 do { responses <- mapM (handleCommand sessionStateRef) commands
    ;  if all (== ViewUpdate) responses
       then return ViewUpdate
       else error "Multiple commands must all result in ViewUpdate at the moment"
    } -- TODO: think of a way to handle multiple commands and dialogs etc.
data ServerResponse = ViewUpdate | Alert String | Confirm String deriving (Show, Eq)

handleCommand :: SessionStateRef -> Command -> IO ServerResponse
handleCommand sessionStateRef Init =
   do { (db, rootView, _) <- readIORef sessionStateRef
      ; putStrLn "Init"
      ; return ViewUpdate
      }
handleCommand sessionStateRef Refresh =
   do { (db, rootView, pendingEdit) <- readIORef sessionStateRef
      ; putStrLn "Refresh"
      ; let rootView' = loadView db (mkViewMap rootView) rootView
      ; writeIORef sessionStateRef (db, rootView', pendingEdit)
      ; return ViewUpdate
      }
handleCommand sessionStateRef Test =
   do { (db, rootView, _) <- readIORef sessionStateRef
      ; return ViewUpdate
      }
handleCommand sessionStateRef (SetC id value) =
   do { (db, rootView, pendingEdit) <- readIORef sessionStateRef      
      ; putStrLn $ show id ++ " value is " ++ show value

      ; let rootView' = replace (Map.fromList [(Id id, value)]) (assignIds rootView)
      ; putStrLn $ "Updated rootView:\n" ++ show rootView'
      ; let db' = save rootView' db
            -- TODO: check if mkViewMap has correct arg
            rootView'' = loadView db' (mkViewMap rootView') rootView'
      -- TODO: instead of updating all, just update the one that was changed
      ; writeIORef sessionStateRef (db', rootView'', pendingEdit)
      ; return ViewUpdate
      }
handleCommand sessionStateRef (ButtonC id) =
   do { (db, rootView, pendingEdit) <- readIORef sessionStateRef
      ; putStrLn $ "Button " ++ show id ++ " was clicked"
      ; let Button _ act = getButtonById (Id id) (assignIds rootView)

 --     ; case act of
 --         ViewEdit i _ -> putStrLn $ "View edit on "++show i
      ; response <- performEditCommand sessionStateRef act
          
      ; return response
      }
handleCommand sessionStateRef ConfirmDialogOk =
   do { (db, rootView, pendingEdit) <- readIORef sessionStateRef
      ; writeIORef sessionStateRef (db, rootView, Nothing) -- clear it, also in case of error
      ; response <- case pendingEdit of
                      Nothing -> error "ConfirmDialogOk event without active dialog"
                      Just ec -> performEditCommand sessionStateRef ec
      ; return response
      }

performEditCommand sessionStateRef command =
 do { (db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; case command of  
            AlertEdit str -> return $ Alert str
            ConfirmEdit str ec -> 
             do { writeIORef sessionStateRef (db, rootView, Just ec)
                ; return $ Confirm str
                }
            _ ->
             do { (db', rootView') <-
                  case command of
                    DocEdit docedit -> 
                      let db' = docedit db
                      in  return (db', loadView db' (mkViewMap rootView) rootView)
                    ViewEdit i viewedit -> 
                      let wv = getWebViewById i rootView
                          wv' = viewedit wv
                          rootView' = replaceWebViewById i wv' rootView 
                          rootView'' = loadView db (mkViewMap rootView') rootView'
                                       -- TODO: check if mkViewMap has correct arg
                                       --       make function for loading rootView
                      in   do { return (db, rootView'')
                              }
                ; writeIORef sessionStateRef (db', rootView', pendingEdit)

                ; return ViewUpdate
                }

    }

safeRead s = case reads s of
               [(x, "")] -> Just x
               _         -> Nothing
