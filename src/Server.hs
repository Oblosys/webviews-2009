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
import Data.Map (Map)
import qualified Data.Map as Map 
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

initState = (theDatabase, mkRootView theDatabase Map.empty, Nothing)

server =
 do { stateRef <- newIORef initState
    ; simpleHTTP (Conf webViewsPort Nothing) $ debugFilter $ msum (handlers stateRef)
    }

{-
handle:
http://<server url>/                    response: <executable dir>/WebViews.html
http://<server url>/favicon.ico         response: <executable dir>/favicon.icome>
http://<server url>/handle?commands=Commands ..                    
                                        response: from handleCommands
-}
handlers :: IORef (Database, WebView, Maybe EditCommand) -> [ServerPartT IO Response]
handlers stateRef = 
  [ exactdir "/" $
      do { liftIO $ putStrLn "Root requested"
         ; fileServe ["WebViews.html"] "."
         }
  , dir "favicon.ico"
        (methodSP GET $ fileServe ["favicon.ico"] ".")
  , dir "img" $
        fileServe [] "img"  
  , dir "handle" $ withData (\cmds -> methodSP GET $
     do { responseHtml <- liftIO $ Control.Exception.catch 
          (do { putStrLn $ "Received data" ++ show cmds
    

              ; response <- handleCommands stateRef cmds
              ; responseHtml <- case response of
                  ViewUpdate ->
                   do { (db, rootView, _) <- readIORef stateRef
                      ; let responseHtml = thediv ! [identifier "updates"] <<
                                       updateReplaceHtml "root" 
                                         (mkDiv "root" $ present $ assignIds rootView)
                      ; putStrLn $ "\n\n\n\ncmds = "++show cmds
                      ; putStrLn $ "rootView:\n" ++ show (assignIds rootView)
                      ; putStrLn $ "database:\n" ++ show db
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
              }) $ \(exc :: SomeException) ->
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
              
        ; ok $ toResponse $ responseHtml
        })
  ]
{- TODO: why does exactdir "/handle" not work?
   TODO: fix syntax error in command

-}


evaluateDbAndRootView stateRef =
 do { dbRootView <- liftIO $ readIORef stateRef
    ; seq (length $ show dbRootView) $ return ()
    }

instance FromData Commands where
  fromData = liftM readCommand (look "commands")

readCommand s = case reads s of
                  [(cmds, "")] -> cmds
                  _            -> SyntaxError s
 
-- handle each command in commands and send the updates back
handleCommands stateRef (SyntaxError cmdStr) =
  error $ "Syntax error in commands from client: "++cmdStr 
handleCommands stateRef (Commands [command]) = handleCommand stateRef command
handleCommands stateRef (Commands commands) = 
 do { responses <- mapM (handleCommand stateRef) commands
    ;  if all (== ViewUpdate) responses
       then return ViewUpdate
       else error "Multiple commands must all result in ViewUpdate at the moment"
    } -- TODO: think of a way to handle multiple commands and dialogs etc.
data ServerResponse = ViewUpdate | Alert String | Confirm String deriving (Show, Eq)

handleCommand :: IORef (Database, WebView, Maybe EditCommand) -> Command -> IO ServerResponse
handleCommand stateRef Init =
   do { (db, rootView,_) <- readIORef stateRef
      ; putStrLn "Init"
      ; return ViewUpdate
      }
handleCommand stateRef Test =
   do { (db, rootView,_) <- readIORef stateRef
      ; return ViewUpdate
      }
handleCommand stateRef (SetC id value) =
   do { (db, rootView,mc) <- readIORef stateRef      
      ; putStrLn $ show id ++ " value is " ++ show value

      ; let rootView' = replace (Map.fromList [(Id id, value)]) (assignIds rootView)
      ; putStrLn $ "Updated rootView:\n" ++ show rootView'
      ; let db' = save rootView' db
            -- TODO: check if mkViewMap has correct arg
            rootView'' = loadView db' (mkViewMap rootView') rootView'
      -- TODO: instead of updating all, just update the one that was changed
      ; writeIORef stateRef (db',rootView'',mc)
      ; return ViewUpdate
      }
handleCommand stateRef (ButtonC id) =
   do { (db, rootView, mc) <- readIORef stateRef
      ; putStrLn $ "Button " ++ show id ++ " was clicked"
      ; let Button _ act = getButtonById (Id id) (assignIds rootView)

 --     ; case act of
 --         ViewEdit i _ -> putStrLn $ "View edit on "++show i
      ; response <- performEditCommand stateRef act
          
      ; return response
      }
handleCommand stateRef ConfirmDialogOk =
   do { (db, rootView,mc) <- readIORef stateRef
      ; writeIORef stateRef (db, rootView, Nothing) -- clear it, also in case of error
      ; response <- case mc of
                      Nothing -> error "ConfirmDialogOk event without active dialog"
                      Just ec -> performEditCommand stateRef ec
      ; return response
      }

performEditCommand stateRef command =
 do { (db, rootView, mc) <- readIORef stateRef
    ; case command of  
            AlertEdit str -> return $ Alert str
            ConfirmEdit str ec -> 
             do { writeIORef stateRef (db, rootView, Just ec)
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
                ; writeIORef stateRef (db', rootView', mc)

                ; return ViewUpdate
                }

    }