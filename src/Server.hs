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
import qualified Data.ByteString.Char8 as Bytestring
import qualified Codec.Binary.Base64.String as Base64

import Types
import Generics
import Database
import Views


webViewsPort = 8085

users :: Map String String
users = Map.fromList [("martijn", "p"), ("anonymous", "")] 
-- TODO: maybe this can be (a special) part of db?

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

type SessionState = (User, Database, WebView, Maybe EditCommand)

type SessionStateRef = IORef SessionState

server =
 do { serverSessionId <- epochTime
    ; globalStateRef <- newIORef initGlobalState
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
        ; fileServe ["WebViews.html"] "."
        }
  , dir "favicon.ico" $
      methodSP GET $ fileServe ["favicon.ico"] "."
  , dir "img" $
      fileServe [] "img"  
--  , dir "authenticate" $ authenticate
--  , dir "unauthenticate" $ unauthenticate 
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
 
createNewSessionState globalStateRef serverInstanceId = 
 do { (database, sessions,sessionCounter) <- liftIO $ readIORef globalStateRef
    ; let sessionId = sessionCounter
    ; lputStrLn $ "New session: "++show sessionId
    ; addCookie 3600 (mkCookie "webviews" $ show (serverInstanceId, sessionId))
    -- cookie lasts for one hour
      
    ; let newSession = (Nothing, mkRootView Nothing theDatabase sessionId Map.empty, Nothing)
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
                             Just (user, rootView, pendingEdit) -> return (user, database, rootView, pendingEdit)
    ; liftIO $ newIORef sessionState
    }                        

storeSessionState globalStateRef sessionId sessionStateRef =
 do { (_, sessions, sessionCounter) <- liftIO $ readIORef globalStateRef
    ; (user', database', rootView', pendingEdit') <- liftIO $ readIORef sessionStateRef
    ; let sessions' = IntMap.insert sessionId (user', rootView', pendingEdit') sessions                                          
    ; liftIO $ writeIORef globalStateRef (database', sessions', sessionCounter)
    }
 
sessionHandler :: SessionStateRef -> Commands -> ServerPart Html
sessionHandler sessionStateRef cmds = {- myAuth `mplus` -} {-
 do { mAuthHeader <- getHeaderM "authorization"
    ; let (s1,s2) = case mAuthHeader of
                           Nothing         -> ("-","-") 
                           Just authHeader -> parseHeader authHeader    
    ; liftIO $ print $ s1 ++ " " ++ s2
    ; unauthorized ()
    ; setHeaderM "WWW-Authenticate" "Basic realm=\"WebViews\""
    ; return $ toResponse $ stringToHtml "bla"
    }
-}
     liftIO $  
 do { putStrLn $ "Received commands" ++ show cmds
    ; response <- handleCommands sessionStateRef cmds
    ; responseHtml <- case response of
        ViewUpdate ->
         do { (_, db, rootView, _) <- readIORef sessionStateRef
            ; let responseHtml = thediv ! [identifier "updates"] <<
                             updateReplaceHtml "root" 
                               (mkDiv "root" $ present $ assignIds rootView)
            --; putStrLn $ "rootView:\n" ++ show (assignIds rootView)
            --; putStrLn $ "database:\n" ++ show db
            ; putStrLn $ "\n\n\nresponse = \n" ++ show responseHtml
            --; putStrLn $ "Sending response sent to client: " ++
            --              take 10 responseHTML ++ "..."
            ; seq (length (show responseHtml)) $ return ()
            ; case cmds of
                (Commands [SetC 10 _]) -> 
                  return $ thediv ! [identifier "updates"] <<
                    thediv![strAttr "op" "special", strAttr "targetId" "root" ] <<   
                      (mkDiv "root" $ present $ assignIds rootView)
                _ -> return responseHtml
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
{-        Authenticate  -> 
          return $ thediv ! [identifier "updates"] <<
                     (thediv![ strAttr "op" "authenticate"
                             ] << "") 
-}        
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
--    }
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
    ;  case dropWhile (== ViewUpdate) responses of
         []         -> return ViewUpdate
         [response] -> return response
         _          -> error "Non View update commmand followed by other commands"
                    -- probably okay if they are all ViewUpdates
    } -- TODO: think of a way to handle multiple commands and dialogs etc.
data ServerResponse = ViewUpdate | Alert String | Confirm String {- | Authenticate -} deriving (Show, Eq)

handleCommand :: SessionStateRef -> Command -> IO ServerResponse
handleCommand sessionStateRef Init =
 do { (_, db, rootView, _) <- readIORef sessionStateRef
    ; putStrLn "Init"
    ; return ViewUpdate
    }
handleCommand sessionStateRef Refresh =
 do { putStrLn "Refresh"
    ; reloadRootView sessionStateRef
    ; return ViewUpdate
    }
handleCommand sessionStateRef Test =
 do { (user, db, rootView, _) <- readIORef sessionStateRef
    ; return ViewUpdate
    }
handleCommand sessionStateRef (SetC id value) =
 do { (user, db, rootView, pendingEdit) <- readIORef sessionStateRef      
    ; putStrLn $ show id ++ " value is " ++ show value

    ; let rootView' = replace (Map.fromList [(Id id, value)]) (assignIds rootView)
    ; putStrLn $ "Updated rootView:\n" ++ show rootView'
    ; let db' = save rootView' db
      -- TODO: check if mkViewMap has correct arg
    -- TODO: instead of updating all, just update the one that was changed
    ; writeIORef sessionStateRef (user, db', rootView', pendingEdit)
    ; reloadRootView sessionStateRef
    ; return ViewUpdate
    }
handleCommand sessionStateRef (ButtonC id) =
 do { (user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; putStrLn $ "Button " ++ show id ++ " was clicked"
    ; let Button _ act = getButtonById (Id id) (assignIds rootView)

    ; response <- performEditCommand sessionStateRef act
          
    ; return response
    }
handleCommand sessionStateRef ConfirmDialogOk =
 do { (user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; writeIORef sessionStateRef (user, db, rootView, Nothing) -- clear it, also in case of error
    ; response <- case pendingEdit of
                    Nothing -> error "ConfirmDialogOk event without active dialog"
                    Just ec -> performEditCommand sessionStateRef ec
    ; return response
    }
 
reloadRootView :: SessionStateRef -> IO ()
reloadRootView sessionStateRef =
 do { (user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; let rootView' = loadView user db (mkViewMap rootView) rootView
    ; writeIORef sessionStateRef (user, db, rootView', pendingEdit)
    } 
 
performEditCommand sessionStateRef command =
 do { (user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; case command of  
            AlertEdit str -> return $ Alert str
            ConfirmEdit str ec -> 
             do { writeIORef sessionStateRef (user, db, rootView, Just ec)
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
                      in  return (db, rootView')
                              
                ; writeIORef sessionStateRef (user, db', rootView', pendingEdit)
                ; reloadRootView sessionStateRef
 
                ; return ViewUpdate
                }

    }

safeRead s = case reads s of
               [(x, "")] -> Just x
               _         -> Nothing


-- TODO id's may not be right. what if views changed and the fields get assigned different id's
-- than when button was created?
authenticate sessionStateRef userEStringId passwordEStringId =
 do { (user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; let mUserName = getEStringByIdRef userEStringId rootView
          mEnteredPassword = getEStringByIdRef passwordEStringId rootView
    ; case (mUserName, mEnteredPassword) of
        (Just userName, Just enteredPassword) ->
           if Map.lookup userName users == Just enteredPassword 
             then 
              do { putStrLn $ "User "++userName++" authenticated"
                 ; writeIORef sessionStateRef (Just userName, db, rootView, pendingEdit)
                 ; reloadRootView sessionStateRef
                 ; return ViewUpdate
                 }
             else 
              do { putStrLn $ "User "++userName++" entered a wrong password"
                 ; return $ Alert "Incorect password"
                 }
        _ -> error $ "Internal error: at least one referenced Id not in rootView" ++
                     show [userEStringId, passwordEStringId]
    }
logout sessionStateRef =
 do { (user, db, rootView, pendingEdit) <- readIORef sessionStateRef
    ; writeIORef sessionStateRef (Nothing, db, rootView, pendingEdit)
    ; reloadRootView sessionStateRef
    ; return ViewUpdate
    }  

{- old http authentication

authenticate = myAuth' `mplus` (return $ toResponse $ stringToHtml "tralalie")  
unauthenticate =
 do { unauthorized ()
    --; setHeaderM "WWW-Authenticate" "Basic realm=\"WebViews\""
    ; return $ toResponse $ stringToHtml "Not Authorized"
    }
  
myAuth' :: ServerPart Response
myAuth' = fmap toResponse myAuth

myAuth :: ServerPart Html
myAuth = basicAuth' "WebViews"
             (Map.fromList [("martijn", " "),("m", " ")]) (return $ stringToHtml "Login Failed")

basicAuth' realmName authMap unauthorizedPart =
    do
        let validLogin name pass = Map.lookup name authMap == Just pass  
        authHeader <- getHeaderM "authorization"
        case authHeader of
            Nothing -> err
            Just x  -> case parseHeader x of 
                (name, ':':pass) | validLogin name pass -> mzero
                                   | otherwise -> err
                _                              -> err
    where
        err = do
            unauthorized ()
            setHeaderM headerName headerValue
            unauthorizedPart
        headerValue = "Basic realm=\"" ++ realmName ++ "\""
        headerName  = "WWW-Authenticate"



parseHeader :: Bytestring.ByteString -> (String,String)
parseHeader = break (':'==) . Base64.decode . Bytestring.unpack . Bytestring.drop 6
        
-}