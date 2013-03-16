{-# LANGUAGE ScopedTypeVariables #-}
module Server where

import Happstack.Server
import System.IO
import Data.List
import Data.List.Split
import Data.Generics (Typeable)
import Data.Maybe
import Data.IORef
import Data.ByteString.Char8 (unpack)
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import System.Time (getClockTime)
import System.Posix.Time
import System.Posix.Types
import System.Exit
import System.Environment (getArgs)
import BlazeHtml hiding (dir, method)
import Control.Exception
import qualified Data.ByteString.Char8 as Bytestring
import qualified Codec.Binary.Base64.String as Base64

import Types
import Generics
import WebViewPrim
import Incrementality
import HtmlLib
import ObloUtils
import Utils


{-
see if passing down dbFilename, db and users can be improved (maybe in state?)
             

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

initGlobalState db = (db, IntMap.empty, 0)

type GlobalStateRef db = IORef (GlobalState db)

type ServerInstanceId = EpochTime

type SessionCounter = Int

type Sessions db = IntMap (User, UntypedWebView db, Maybe [Maybe (EditM db ())], String, HashArgs)

server :: (Show db, Read db, Eq db, Typeable db) =>
          Int -> String -> RootViews db -> [String] -> String -> IO (db) -> Map String (String, String) -> IO ()
server portNr title rootViews scriptFilenames dbFilename mkInitialDatabase users =
 do { hSetBuffering stdout NoBuffering -- necessary to run server in Eclipse
    ; time <- getClockTime
    ; args <- getArgs
    ; let debug = case args of
                    []            -> True
                    ["--nodebug"] -> False
                    _             -> error "Incorrect parameters: only 'nodebug' is allowed"
        
    ; putStrLn $ "\n\n### Started WebViews server "++show title++" (port "++show portNr++")\n"++show time ++"\n"++
                 "Debugging: "++(if debug then "ON" else "OFF")++"\n\n"
    ; serverSessionId <- epochTime

    ; mDb <-
       do { fh <- openFile dbFilename ReadMode
          ; dbStr <- hGetContents fh
          ; seq (length dbStr) $ return ()
          ; hClose fh
          ; return $ readMaybe dbStr
          } `Control.Exception.catch` \exc ->
       do { putStrLn $ "Problem opening "++dbFilename++":\n"
          ; putStrLn $ "Exception "++ show (exc :: SomeException)
          ; putStrLn $ "\nUsing initial database."
          ; db <- mkInitialDatabase
          ; return $ Just db
          }

    ; db <- case mDb of -- if the database exists but cannot be read, something is wrong and we exit to prevent overwriting it
                       Just db -> return db
                       Nothing -> do { putStrLn $ "Database file "++dbFilename++" cannot be read, exiting server.\n"
                                     ; exitWith $ ExitFailure 1
                                     }

    ; globalStateRef <- newIORef $ initGlobalState db

    ; simpleHTTP nullConf { port = portNr, logAccess = Nothing {-Just logWebViewAccess-} } $
        msum (handlers debug title rootViews scriptFilenames dbFilename db users serverSessionId globalStateRef)
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

readCommand s = case readMaybe s of
                  Just cmds -> cmds
                  Nothing   -> SyntaxError s

instance FromData Int where
  fromData = liftM readInt (look "requestId")

readInt s = fromMaybe (-1) (readMaybe s)

handlers :: (Show db, Eq db, Typeable db) => Bool -> String -> RootViews db -> [String] -> String -> db -> Map String (String, String) -> ServerInstanceId -> GlobalStateRef db -> [ServerPart Response]
handlers debug title rootViews scriptFilenames dbFilename db users serverSessionId globalStateRef = 
  (do { msum [ dir "favicon.ico" $  serveDirectory DisableBrowsing [] "favicon.ico"
             , dir "scr" $ msum [ serveDirectory DisableBrowsing [] "scr"
                                , uriRest $ \pth -> notFound $ toResponse $ "File not found: scr/"++pth
                                ]
             , dir "img" $ do { neverExpires
                              ; serveDirectory DisableBrowsing [] "img"
                              }
             ]
      }) :
  [ dir "handle" $ 
      withData (\cmds -> do { time <- io getClockTime
           ; clientIp <- getClientIp 
                            ; requestIdData <- getData
                            ; requestId <- case requestIdData of
                                            Right i |  i/=(-1)  -> return i
                                            Right i | otherwise -> do { io $ putStrLn $ "Unreadable requestId from " ++ clientIp ++ ", "++show time;    mzero }
                                            Left err            -> do { io $ putStrLn $ "No requestId in request from " ++ clientIp ++ ", "++show time; mzero }
                                
                            ; io $ putStrLn $ "RequestId " ++ show (requestId :: Int) ++ " (" ++ clientIp ++ "), "++show time
                            ; method GET
                            ; nullDir
                            ; fmap (setHeader "Cache-Control" "no-cache") $ 
                                session debug rootViews dbFilename db users serverSessionId globalStateRef requestId cmds
                            })
  , serveRootPage -- this generates an init event, which will handle hash arguments
  ] 
 where serveRootPage :: ServerPart Response
       serveRootPage =
        do { time <- io getClockTime
           ; clientIp <- getClientIp 
           ; Request{rqHeaders=hdrs} <- askRq
           ; io $ putStrLn $ "Root requested (" ++ clientIp ++ "), "++show time
           ; io $ putStrLn $ "User agent: " ++ maybe "<no user agent header>" unpack (getHeader "user-agent" hdrs) ++ "\n\n"
           ; templateStr <- io $ readUTFFile $ "htmlTemplates/WebViews.html"
           ; let linksAndScripts = concatMap mkScriptLink scriptFilenames
           ; let debugVal = if debug then "true" else "false"
           ; let htmlStr = substitute [("TITLE",title),("LINKSANDSCRIPTS",linksAndScripts),("DEBUG", debugVal)] templateStr
           ; ok $ -- be careful with redirected urls (e.g. webviews.oblomov.com) since they clear these headers
                  setHeader "Cache-Control" "no-cache" $
                  setHeader "Content-Type" "text/html; charset=utf-8" $
                  setHeader "X-Frame-Options" "ALLOWALL" $ -- ALLOW apparently is not official, but it works
                  toResponse htmlStr
           } 
           
       getClientIp :: ServerPart String
       getClientIp =
        do { Request{rqHeaders=hdrs,rqPeer = (actualClientIp,_)} <- askRq
           ; return $ case getHeader "x-forwarded-for" hdrs of
                        Nothing                -> actualClientIp
                        Just forwardedClientIp -> unpack forwardedClientIp
           }
       mkScriptLink filename = case reverse . takeWhile (/='.') . reverse $ filename of
                                 "js"  -> "  <script type=\"text/javascript\" src=\"/scr/js/"++filename++"\"></script>\n"
                                 "css" -> "  <link href=\"/scr/css/"++filename++"\" rel=\"stylesheet\" type=\"text/css\" />\n"
                                 ext   -> error $ "Unhandled script extension: "++filename
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

session :: (Show db, Eq db, Typeable db) => Bool -> RootViews db -> String -> db -> Map String (String, String) -> ServerInstanceId -> GlobalStateRef db -> Int -> Commands -> ServerPart Response
session debug rootViews dbFilename db users serverInstanceId globalStateRef requestId cmds =
 do { mCookieSessionId <- parseCookieSessionId serverInstanceId
      
--        ; lputStrLn $ show rq
        ; sessionId <- case mCookieSessionId of 
            Nothing  -> createNewSessionState debug db globalStateRef serverInstanceId
            Just key -> do { --lputStrLn $ "Existing session "++show key
                           ; return key
                           }
             
        ; mSessionStateRef <- retrieveSessionState globalStateRef sessionId 
        
        ; responseHtml <-
            case mSessionStateRef of                         -- don't have a requestId, so use -1 
              Nothing              -> io $ do { (_, sessions,_) <- readIORef globalStateRef
    ;                                         ; raiseClientException (-1) $ "Internal error: Session "++show sessionId++
                                                                            " not found in " ++ show (IntMap.keys sessions)
                                              }
              Just sessionStateRef -> 
               do { responseHtml <- sessionHandler rootViews dbFilename db users sessionStateRef requestId cmds              
                  ; storeSessionState globalStateRef sessionId sessionStateRef
                  ; return responseHtml
                  }
        
        ; io $ putStrLn "Done\n\n"
        ; ok $ toResponse $ responseHtml
        }
 
parseCookieSessionId :: ServerInstanceId -> ServerPart (Maybe SessionId)
parseCookieSessionId serverInstanceId = 
 do { rq <- askRq
             
    ; let cookieMap = rqCookies rq
    ; let mCookieSessionId = case lookup "webviews" cookieMap of
                      Nothing -> Nothing -- * no webviews cookie on the client
                      Just c  -> case readMaybe (cookieValue c) :: Maybe (EpochTime, Int) of
                                   Nothing               -> Nothing -- * ill formed cookie on client
                                   Just (serverTime, key) -> 
                                     if serverTime /= serverInstanceId
                                     then Nothing  -- * cookie from previous WebViews run
                                     else Just key -- * correct cookie for this run
    ; return mCookieSessionId
    }

mkInitialRootView :: Typeable db => db -> IO (UntypedWebView db)
mkInitialRootView db = do { wv <- runWebView Nothing db Map.empty [] 0 (-1) "" [] $ fmap UntypedWebView $ mkWebView (\_ _ -> return InitialView) 
                          ; return wv
                          }
-- this creates a WebView with stubid 0 and id 1
-- for now, we count on that in the client
-- TODO: change this to something more robust
-- todo: use different id

-- Raise an exception in the browser and report to stdout. (necessary because we cannot catch exceptions in the
-- ServerPart monad)
raiseClientException :: Int -> String -> IO Html
raiseClientException requestId msg =
 do { let exceptionText = 
            "\n\n\n\n################################################################################\n\n\n" ++
            msg ++ "\n\n\n" ++
            "################################################################################\n\n" 
    
    ; putStrLn exceptionText
    ; return $ div_ ! id_ "updates" ! strAttr "responseId" (show requestId) $
                (div_ ! strAttr "op" "exception"
                      ! strAttr "text" exceptionText
                      $ noHtml)                       
    }

createNewSessionState :: Typeable db => Bool -> db -> GlobalStateRef db -> ServerInstanceId -> ServerPart SessionId
createNewSessionState debug db globalStateRef serverInstanceId = 
 do { (database, sessions,sessionCounter) <- io $ readIORef globalStateRef
    ; let sessionId = sessionCounter
    ; io $ putStrLn $ "New session: "++show sessionId
    ; addCookie Session (mkCookie "webviews" $ show (serverInstanceId, sessionId))
    -- cookie lasts for one hour
 
    ; initialRootView <- io $ mkInitialRootView db
                        
                       -- for debugging, begin with user martijn  
    ; let newSession = (if debug then Just ("martijn", "Martijn Schrage") else Nothing, initialRootView, Nothing, "", [])
    ; let sessions' = IntMap.insert sessionId newSession sessions
   
    ; io $ writeIORef globalStateRef (database, sessions', sessionCounter + 1)
    
    ; return sessionId
    }
 
retrieveSessionState :: GlobalStateRef db -> SessionId -> ServerPart (Maybe (SessionStateRef db))
retrieveSessionState globalStateRef sessionId =
 do { (database, sessions, sessionCounter) <- io $ readIORef globalStateRef
    --; lputStrLn $ "\n\nNumber of active sessions: " ++ show sessionCounter                                          
    ; case IntMap.lookup sessionId sessions of
        Nothing                                         -> return Nothing
        Just (user, rootView, dialogCommands, rootViewName, hashArgs) -> 
         do { let sessionState = SessionState sessionId user database rootView dialogCommands rootViewName hashArgs
            ; sessionStateRef <-io $ newIORef sessionState
            ; return $ Just sessionStateRef
            }
    }      
 
storeSessionState :: GlobalStateRef db -> SessionId -> SessionStateRef db -> ServerPart ()
storeSessionState globalStateRef sessionId sessionStateRef =
 do { (_, sessions, sessionCounter) <- io $ readIORef globalStateRef
    ; SessionState _ user' database' rootView' dialogCommands' rootViewName hashArgs <- io $ readIORef sessionStateRef
    ; let sessions' = IntMap.insert sessionId (user', rootView', dialogCommands', rootViewName, hashArgs) sessions                                          
    ; io $ writeIORef globalStateRef (database', sessions', sessionCounter)
    }
 
sessionHandler :: (Show db, Eq db, Typeable db) => RootViews db -> String -> db -> Map String (String, String) -> SessionStateRef db -> Int -> Commands -> ServerPart Html
sessionHandler rootViews dbFilename db users sessionStateRef requestId (SyntaxError cmdStr) =
 io $ raiseClientException requestId $ "Syntax error in commands from client: "++cmdStr 
sessionHandler rootViews dbFilename db users sessionStateRef requestId (Commands allCmds) = io $  
 do { --putStrLn $ "Received commands" ++ show cmds
    
    ; let isInitCommand (Init _ _) = True
          isInitCommand _          = False
    
    -- If one of the commands is Init, we remove the commands in front of the last Init and start with a new initial root view 
    ; cmds <- 
        case break isInitCommand $ reverse $ allCmds of
          (nonInitCmds, []) -> return $ reverse nonInitCmds
          (nonInitCmds, initCmd:_) ->
           do { initialRootView <- mkInitialRootView db
              ; setRootView sessionStateRef initialRootView
              ; return $ initCmd : reverse nonInitCmds
              }

    ; SessionState _ _ oldDb oldRootView _ _ _ <- readIORef sessionStateRef
    
    ; responseHtml <- handleCommands rootViews dbFilename db users sessionStateRef oldRootView cmds
    --; putStrLn $ show responseHtml
    
    ; return $ div_ ! id_ "updates" ! strAttr "responseId" (show requestId) $ concatHtml responseHtml
    } `Control.Exception.catch` \exc -> raiseClientException requestId $ "Exception: " ++ show (exc :: SomeException)
    


data ServerResponse = ServerResponse [String] (Maybe (Html, [(String,Bool)])) deriving (Show, Eq)
                  
{-
After processing the commands, a view update response is sent back, followed by the concatenated scripts from all commands.
A Confirm command produces a modal dialog, so we ignore all commands following it. Since handle command modifies the root webview
as well as the database, we need to end traversing the list on a Confirm command.

Note that EvalJS and Confirm also cause a view update.
-}    

handleCommands :: forall db . (Eq db, Show db) =>
                  RootViews db -> String -> db -> Map String (String, String) -> SessionStateRef db -> UntypedWebView db -> [Command] ->
                  IO [Html]
handleCommands rootViews dbFilename db users sessionStateRef oldRootView cmds = handleCommands' [] cmds
 where handleCommands' allScripts [] = 
        do { let evalHtml = if null allScripts then [] else mkEvalJSResponseHtml $ concat allScripts
           ; viewUpdateHtml <- mkViewUpdateResponseHtml sessionStateRef dbFilename db oldRootView
           ; return $ viewUpdateHtml ++ evalHtml -- update the view and evaluate the collected scripts
           }
       handleCommands' allScripts (command:commands) = 
        do { ServerResponse scriptLines mDialog <- handleCommand rootViews users sessionStateRef command
           ; let allScripts' = allScripts ++ scriptLines
           ; case mDialog of
               Nothing -> handleCommands' allScripts' commands
               Just (contents, buttonNames) ->
                do { updateAndEvalHtml <- handleCommands' allScripts' [] -- No recursive call: we ignore rest of commands but
                                                                         -- do create html for update + eval with a [] call 
                   ; return $ mkConfirmDialogResponseHtml contents buttonNames
                   }
           }


mkEvalJSResponseHtml :: String -> [Html]
mkEvalJSResponseHtml script = [ div_ ! strAttr "op" "eval" $ toHtml script ] 


-- <div op=dialog><div class=contents> ..dialog html..</div><div class=button name=buttonName0 command=true/false /> ... </div>
mkConfirmDialogResponseHtml :: Html -> [(String,Bool)] -> [Html]
mkConfirmDialogResponseHtml dialogHtml buttonNames = -- the Bool specifies whether the associated button has a server-side command 
  [ div_ ! strAttr "op" "dialog" $                   -- (otherwise it's a cancel button that only hides the dialog)
     (div_ ! class_ "contents" $ dialogHtml) +++ 
     concatHtml [ div_ ! class_ "button" ! strAttr "name" name ! strAttr "command" (if command then "true" else "false") $ noHtml 
                | (name,command) <- buttonNames ]
  ] 


mkViewUpdateResponseHtml :: (Eq db, Show db) => SessionStateRef db -> String -> db -> UntypedWebView db -> IO [Html]
mkViewUpdateResponseHtml sessionStateRef dbFilename db (UntypedWebView oldRootView) =
         do { 
 
            ; UntypedWebView rootViewWithoutIds <- getRootView sessionStateRef
              -- this is the modified rootView                      
                                    
            -- save the database if there was a change
            ; SessionState{getSStateDb=db'} <- readIORef sessionStateRef
            ; if db /= db' then 
               do { fh <- openFile dbFilename WriteMode
                  ; hPutStr fh $ show db'
                  ; hClose fh
                  }
               else return ()
            
            ; let rootView = assignAllUniqueIds oldRootView rootViewWithoutIds 
            
            --; putStrLn "seq'ing rootView"
            ; seq (length (show rootView)) $ return ()
            --; putStrLn "seq'ed rootView"
            
            ; setRootView sessionStateRef $ UntypedWebView rootView
              
              
            ; (responseHtml, rootView') <- mkIncrementalUpdates oldRootView rootView
            -- rootView' has different id's (the ones that were not updated and hence are
            -- restored to their previous values)
                                           
            --; putStrLn $ "View tree:\n" ++ drawWebNodes (WebViewNode rootView') 
            --; putStrLn $ "rootView':\n" ++ show rootView'
            ; setRootView sessionStateRef $ UntypedWebView rootView'
            --; putStrLn $ "database:\n" ++ show db
            --; putStrLn $ "\n\n\nresponse = \n" ++ show responseHtml
            --; putStrLn $ "View tree':\n" ++ drawWebNodes (WebViewNode rootView') 
            --; putStrLn $ "Sending response sent to client: " ++
            --              take 10 responseHTML ++ "..."
            --; putStrLn "seq'ing response"
            ; seq (length (show responseHtml)) $ return ()
            --; putStrLn "end session handler"
            ; return $ responseHtml
            }

handleCommand :: forall db . RootViews db -> Map String (String, String) -> SessionStateRef db -> Command -> IO ServerResponse
handleCommand rootViews _ sessionStateRef (Init rootViewName hashArgs) =
 do { putStrLn $ "Init " ++ show rootViewName ++ " " ++ show hashArgs
    ; setSessionRootViewName sessionStateRef rootViewName
    ; setSessionHashArgs sessionStateRef hashArgs
    ; SessionState sessionId user db (UntypedWebView oldRootView) _ _ _ <- readIORef sessionStateRef
    ; rootView <- io $ createRootView rootViews rootViewName hashArgs user db sessionId (mkViewMap oldRootView)
    ; setRootView sessionStateRef rootView
     
    ; return $ ServerResponse [] Nothing
    }
handleCommand rootViews _ sessionStateRef (HashUpdate rootViewName hashArgs) = -- fired when hash hash changed in client
 do { putStrLn $ "HashUpdate " ++ show rootViewName ++ " " ++ show hashArgs
    ; setSessionHashArgs sessionStateRef hashArgs
    ; SessionState sessionId user db (UntypedWebView oldRootView) _ _ _ <- readIORef sessionStateRef
    ; rootView <- io $ createRootView rootViews rootViewName hashArgs user db sessionId (mkViewMap oldRootView)
    ; setRootView sessionStateRef rootView
     
    ; return $ ServerResponse [] Nothing
    } 
handleCommand _ _ sessionStateRef Refresh =
 do { -- putStrLn "Refresh"
    ; reloadRootView sessionStateRef
    ; return $ ServerResponse [] Nothing
    }
handleCommand _ _ sessionStateRef Test =
 do { sState <- readIORef sessionStateRef
    ; return $ ServerResponse [] Nothing
    }
handleCommand _ users sessionStateRef (SetC viewId value) =
 do { SessionState sessionId user db (UntypedWebView rootView) dialogCommands rootViewName hashArgs <- readIORef sessionStateRef      
    ; putStrLn $ "Performing: "++show (SetC viewId value)
    --; putStrLn $ "RootView:\n" ++ show rootView ++"\n\n\n\n\n"
    ; let rootView' = applyUpdates (Map.fromList [(viewId, value)]) (assignIds rootView)
    ; let db' = save rootView' db
    ; writeIORef sessionStateRef $ SessionState sessionId user db' (UntypedWebView rootView') dialogCommands rootViewName hashArgs
    ; reloadRootView sessionStateRef

    --; putStrLn $ "Updated rootView:\n" ++ show rootView'
    ; response <- case mGetAnyWidgetById viewId rootView' :: Maybe (AnyWidget db) of
        Just (TextWidget (TextView _ _ _ _ _ (Just fChangeAction) _))       -> performEdit users sessionStateRef (fChangeAction value) 
        Just (RadioViewWidget (RadioView _ _ _ _ _ (Just fChangeAction)))   -> performEdit users sessionStateRef (fChangeAction $ unsafeRead "Server.handle: radio selection" value) 
        Just (SelectViewWidget (SelectView _ _ _ _ _ (Just fChangeAction))) -> performEdit users sessionStateRef (fChangeAction $ unsafeRead "Server.handle: select selection" value) 
        _                                                                   -> return $ ServerResponse [] Nothing -- Not a widget with a change action
      -- TODO: check if mkViewMap has correct arg
    -- TODO: instead of updating all, just update the one that was changed
    ; return response
    } 
handleCommand _  users sessionStateRef (ButtonC viewId) =
 do { SessionState _ user db (UntypedWebView rootView) dialogCommands _ hashArgs <- readIORef sessionStateRef
    ; let (Button _ txt _ _ _ act) = getButtonByViewId viewId rootView
    ; putStrLn $ "Button #" ++ show viewId ++ ":" ++ txt ++ " was clicked"

    ; response <- performEdit users  sessionStateRef act
          
    ; return response
    }
handleCommand _ users sessionStateRef (SubmitC viewId) =
 do { SessionState _ user db (UntypedWebView rootView) dialogCommandst _ hashArgs <- readIORef sessionStateRef
    ; let TextView _ _ txt _ _ _ mAct = getTextViewByViewId viewId rootView
    ; putStrLn $ "TextView #" ++ show viewId ++ ":" ++ txt ++ " was submitted"

    ; response <- case mAct of 
        Nothing  -> error "Internal error: text field with submission action has no associated action."
        Just act -> performEdit users sessionStateRef act
          
    ; return response
    }
handleCommand _ users sessionStateRef (PerformEditActionC viewId args) =
 do { SessionState _ user db (UntypedWebView rootView) dialogCommands _ hashArgs <- readIORef sessionStateRef
    ; let EditAction _ act = getEditActionByViewId viewId rootView
    ; putStrLn $ "EditAction with ViewId "++show viewId ++ " was executed"

    ; response <- performEdit users sessionStateRef $ act args
          
    ; return response
    }
handleCommand _ users sessionStateRef (DialogButtonPressed nr) = -- TODO
 do { putStrLn $ "Dialog button " ++ show nr ++ " was clicked"
    ; sState <- readIORef sessionStateRef
    ; writeIORef sessionStateRef sState{getSStateDialogCommands = Nothing} -- clear it, also in case of error
    ; response <- case getSStateDialogCommands sState of
                    Nothing -> error "DialogButtonPressed event without active dialog"
                    Just dcs -> if nr < length dcs 
                                then case dcs!!nr of
                                       Just dc -> performEdit users sessionStateRef dc
                                       Nothing -> error $ "DialogButtonPressed for button without edit command: "++show nr 
                                else error "Index of pressed button exceeds number of buttons"
    ; return response
    }
 
reloadRootView :: SessionStateRef db -> IO ()
reloadRootView sessionStateRef =
 do { SessionState sessionId user db (UntypedWebView rootView) dialogCommands rootViewName hashArgs <- readIORef sessionStateRef
    ; 
    ; rootView' <- evalStateT (loadView rootView) (WebViewState user db (mkViewMap rootView) [] 0 sessionId rootViewName hashArgs)
 -- TODO this 0 does not seem right BUG
    ; writeIORef sessionStateRef $ SessionState sessionId user db (UntypedWebView rootView') dialogCommands rootViewName hashArgs
    } 
 
performEdit :: Map String (String, String) -> SessionStateRef db -> EditM db () -> IO ServerResponse
performEdit users sessionStateRef edit  =
 do { sessionState <- readIORef sessionStateRef
    ; let editState = EditState users (getSStateUser sessionState) (getSStateDb sessionState) (getSStateRootView sessionState) (getSStateRootViewName sessionState) (getSStateHashArgs sessionState) [] Nothing
    ; EditState _ user' db' rootView' _ _ scriptLines mDialog <- execStateT edit editState
    -- HashArgs and rootViewName are read-only
    ; writeIORef sessionStateRef sessionState{ getSStateUser = user', getSStateDb = db', getSStateRootView = rootView' }
    ; reloadRootView sessionStateRef
    --; io $ putStrLn $ "javascript:\n" ++ concat scriptLines
    --; io $ putStrLn $ if (isJust mDialog) then "Dialog" else "No dialog"
    ; mDialogResponse <- case mDialog of
        Just (contents, buttons) ->
         do { let (buttonNames, buttonCommands) = unzip buttons
            ; modifyIORef sessionStateRef $ \sstate -> sstate{getSStateDialogCommands = Just buttonCommands}
            ; return $ Just (contents, zip buttonNames (map isJust buttonCommands))
            }
        Nothing -> return Nothing
    ; return $ ServerResponse scriptLines mDialogResponse 
    }
