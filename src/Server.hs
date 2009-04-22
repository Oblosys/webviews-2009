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

{- authorization demo 
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Codec.Binary.Base64.String as Base64
 -}

{-
HAPPS
Server error: Prelude.last: empty list
is the error you get when fileServe cannot find a file

ServerPart is basically a Reader monad for requests

The Ok part of the WebT monad contains a function out that is applied to the responses before
sending them to the client. If the result is of type Response, set/addHeader can be fmapped to
the monad, but it will only do something if the header is not set in the out part of Ok.

Header modifications must therefore be applied to out rather than be fmapped to the monad.
-}

server =
 do { stateRef <- newIORef (theDatabase, mkRootView theDatabase) 
    ; simpleHTTP (Conf 8085 Nothing) $ debugFilter $ msum (handlers stateRef)
    }

{-
server = simpleHTTP (Conf 8080 Nothing) $ myAuth `mplus` return "<html><head><title>blaa</title></head></html>"
     where
         myAuth = basicAuth' "Test"
             (M.fromList [("hello", "world")]) (return "Login Failed")

basicAuth' realmName authMap unauthorizedPart =
    do
        let validLogin name pass = M.lookup name authMap == Just pass
        let parseHeader :: B.ByteString -> (String,String)
            parseHeader = break (':'==) . Base64.decode . B.unpack . B.drop 6
        authHeader <- getHeaderM "authorization"
        case authHeader of
            Nothing -> err
            Just x  -> case parseHeader x of 
                (name, ':':pass) | validLogin name pass -> mzero
                                   | otherwise -> err
                _                                       -> err
    where
        err = do
            unauthorized ()
            setHeaderM headerName headerValue
            unauthorizedPart
        headerValue = "Basic realm=\"" ++ realmName ++ "\""
        headerName  = "WWW-Authenticate"
-}


{-
handle:
http://<server url>/                    response: <proxima executable dir>/../proxima/scripts/Editor.xml
http://<server url>/favicon.ico         response: <proxima executable dir>/img/favicon.ico
http://<server url>/img/<filename>      response: <proxima executable dir>/img/<filename>
http://<server url>/handle?commands=<commands separated by ;>                    
                                        response: from handleCommands

TODO: The proxima server requires that the proxima directory is present for favicon and 
      Editor.xml, these files should be part of a binary distribution.
-}
{-
overrideHeaders :: [(String,String)] -> ServerPart a -> ServerPart a
overrideHeaders headers s =
 do { response <- s
    ; modifyResponse (setHeader "Content-Type" "text/xml")
    ; return s
    } 
-}
{-
modifyResponseSP :: (Response -> Response) -> ServerPart a -> ServerPart a
modifyResponseSP modResp (ServerPartT f) =
  withRequest $ \rq -> modifyResponseW modResp $ f rq
    
modifyResponseW modResp w =
 do { a <- w
    ; modifyResponse modResp
    ; return a
    }
-}    
noCache :: Response -> Response  
noCache = addHeader "Expires" "Mon, 28 Jul 2000 11:24:47 GMT"
-- TODO: figure out if noCache is really necessary, both for editor.xml and handle
-- It does not work for IE
{-
withAgentIsMIE f = withRequest $ \rq -> 
                     (unServerPartT $ f ("MSIE" `isInfixOf` (show $ getHeader "user-agent" rq))) rq
                     -- not the most elegant method of checking for Internet explorer

                     -- IE does not support SVG and XHTML
                     -- XHTML is not a big problem, but for SVG we need an alternative
                     -- Maybe we also need to switch to POST for IE, since it
                     -- cannot handle large queries with GET
  
-}                   
handlers :: IORef (Database, WebView) -> [ServerPartT IO Response]
handlers stateRef = 
  [ {-
   withAgentIsMIE $ \agentIsMIE ->
      (methodSP GET $ do { -- liftIO $ putStrLn $ "############# page request"
                           let setTypeToHTML = if agentIsMIE 
                                               then setHeader "Content-Type" "text/html"
                                               else id
                                           
                         ; modifyResponseSP (noCache. setTypeToHTML) $
                              fileServe [] "../proxima/scripts/Editor.xml" 
                         })
                 
-}
    exactdir "/" $
      do { liftIO $ putStrLn "Root requested"
         ; fileServe ["WebViews.html"] "."
         }
  , dir "favicon.ico"
        (methodSP GET $ fileServe ["favicon.ico"] ".")

  , dir "handle" $
     withData (\cmds -> methodSP GET $
                          do { responseHtml <- liftIO $ Control.Exception.catch 
                               (do { putStrLn $ "Received data" ++ take 20 (show cmds)
                      

                                   ; handleCommands stateRef cmds
                                   ; (db, rootView) <- readIORef stateRef
                                   ; let responseHtml = thediv ! [identifier "updates"] <<
                                                    updateReplaceHtml "root" 
                                                      (mkDiv "root" $ present $ assignIds rootView)
                                   ; putStrLn $ "\n\n\n\ncmds = "++show cmds
                                   ; putStrLn $ "rootView:\n" ++ show (assignIds rootView)
                                   ; putStrLn $ "database:\n" ++ show db
                             --; lputStrLn $ "\n\n\nresponse = \n" ++ show responseHtml
                             --; lputStrLn $ "Sending response sent to client: " ++
                             --              take 10 responseHTML ++ "..."
                             --; modifyResponseW noCache $
                                   ; seq (length (show responseHtml)) $ return ()
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
                             }
                        )
    

  ]
{- TODO: why does exactdir "/handle" not work?
   TODO: fix syntax error in command

-}


evaluateDbAndRootView stateRef =
 do { dbRootView <- liftIO $ readIORef stateRef
    ; seq (length $ show dbRootView) $ return ()
    }

data Commands = Commands String deriving Show

instance FromData Commands where
  fromData = liftM Commands (look "commands")

splitCommands commandStr =
  case break (==';') commandStr of
    ([],[])             -> []
    (_, [])             -> error "Syntax error in commands"
    (command, (_:commandStr')) -> command : splitCommands commandStr'
        
-- handle each command in commands and send the updates back
handleCommands stateRef (Commands commandStr) =
 do { let commands = splitCommands commandStr
    ; putStrLn $ "Received commands:"++ show commands
    
    ; mapM (handleCommand stateRef) commands
    }
    
    
handleCommand stateRef event =
  if "Init" `isPrefixOf` event
  then 
   do { (db, rootView) <- readIORef stateRef
      ; putStrLn "Init"
      ; return ()
      }
  else if "Test" `isPrefixOf` event
  then 
   do { (db, rootView) <- readIORef stateRef
      ; return ()
      }
  else if "Set" `isPrefixOf` event
  then 
   do { (db, rootView) <- readIORef stateRef

      ; putStr "Set "
      ; let (id,value) =
              case break (==',') $ drop 4 event of
                (id, [])       -> (id, "something is very wrong")
                (id, _:rest) -> (id,takeWhile (/=')') rest) 
      
      ; putStrLn $ id ++ "value is " ++ value

      ; let rootView' = replace (Map.fromList [(Id (read id), value)]) (assignIds rootView)
      ; putStrLn $ "Updated rootView:\n" ++ show rootView'
      ; let db' = saveAllViews rootView' db
      ; let rootView'' = loadView db' rootView'
      -- TODO: instead of updating all, just update the one that was changed
      ; writeIORef stateRef (db',rootView'')
     -- ; threadDelay 100000
      ; return ()    
      }
  else if "Button" `isPrefixOf` event
  then 
   do { (db, rootView) <- readIORef stateRef
      ; let id = takeWhile (/=')') $ drop 7 event

      ; putStrLn $ "Button " ++ show id ++ " was clicked"
      ; let Button _ act = getButtonById (Id $ read id) (assignIds rootView)

 --     ; case act of
 --         ViewEdit i _ -> putStrLn $ "View edit on "++show i
      ; let (db', rootView') =
              case act of
                DocEdit docedit -> 
                  let db' = docedit db
                  in  (db', loadView db' rootView)
                ViewEdit i viewedit -> 
                  let wv = getWebViewById i rootView
                      wv' = viewedit wv
                      rootView' = replaceWebViewById i wv' rootView 
                      rootView'' = loadView db rootView'
                  in  (db, rootView'')


      ; writeIORef stateRef (db', rootView')

      ; return ()
      }
  else return ()
 




