module Server where

import Happstack.Server
import Happstack.Helpers hiding (HtmlString)
import Control.Monad
import Control.Monad.Trans
import System.IO
import Data.List
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map 
import Text.Html
import Data.Generics
import Database
import Control.Concurrent

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
 do { docRef <- newIORef database
    ; simpleHTTP (Conf 8080 Nothing) $ debugFilter $ msum (handlers docRef)
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
handlers :: IORef Database -> [ServerPartT IO Response]
handlers docRef = 
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
         ; fileServe ["WebForms.html"] "."
         }
  , dir "favicon.ico"
        (methodSP GET $ fileServe ["favicon.ico"] ".")

  , dir "handle" $
     withData (\cmds -> methodSP GET $ 
                          do { liftIO $ putStrLn $ "Received data" ++ take 20 (show cmds)
                      
                             ; responseHTML <- liftIO $ handleCommands docRef cmds
                             ; liftIO $ putStrLn $ "\n\n\n\ncmds = "++show cmds
                             ; liftIO $ putStrLn $ "\n\n\nresponse = \n" ++ show responseHTML
                        --       ; liftIO $ putStrLn $ "Sending response sent to client: " ++
                        --                             take 10 responseHTML ++ "..."
                         --    ; modifyResponseW noCache $
                             
                             ; ok $ toResponse $ responseHTML
                             }
                        )
    

  ]
{- TODO: why does exactdir "/handle" not work?
   TODO: fix syntax error in command
-}

data Commands = Commands String deriving Show

instance FromData Commands where
  fromData = liftM Commands (look "commands")

splitCommands commandStr =
  case break (==';') commandStr of
    ([],[])             -> []
    (_, [])              -> error "Syntax error in commands"
    (command, (_:commandStr')) -> command : splitCommands commandStr'
        
-- handle each command in commands and send the updates back
handleCommands docRef (Commands commandStr) =
 do { let commands = splitCommands commandStr
    ; putStrLn $ "Received commands:"++ show commands
    
    ; updates <-
        mapM (handleCommand docRef) commands
 


    ; return $ thediv ! [identifier "updates"] << head updates            
    }
    
    
handleCommand docRef event =
  if "Init" `isPrefixOf` event
  then 
   do { doc <- readIORef docRef
      ; putStrLn "Init"
      ; return $ updateReplace "root" $ present database (assignIds doc) -- presentRoot doc
      }
  else if "Test" `isPrefixOf` event
  then 
   do { doc <- readIORef docRef
      ; putStrLn "Test"
      ; return $ updateReplace "somewhere" $ thediv![identifier "root"] << p << "tralalie" 
      }
  else if "Set" `isPrefixOf` event
  then 
   do { doc <- readIORef docRef
      ; putStr "Set "
      ; let (id,value) =
              case break (==',') $ drop 4 event of
                (id, [])       -> (id, "something is very wrong")
                (id, _:rest) -> (id,takeWhile (/=')') rest) 
      
      ; putStrLn $ id ++ "value is " ++ value
      ; let doc' = replace (Map.fromList [(Id (read id), value)]) (assignIds doc)
      ; putStrLn $ "Updated doc:\n" ++ show doc'
      ; writeIORef docRef doc'
--      ; threadDelay 200000
--      ; return $ toHtml ""
      ; return $ updateReplace "root" $ present database (assignIds doc') -- presentRoot doc
      }
  else return $ toHtml ""
 
testElt = thediv![identifier "root"] << (
              p << "bla bla bla"
          +++ p << "bla bloe bla"
          +++ thediv![identifier "somewhere"] << p << "za za za")
updateReplace targetId newElement =
  thediv![strAttr "op" "replace", strAttr "targetId" targetId ] 
    << newElement

mkDiv str elt = thediv![identifier str] << elt

data TestDoc = TestDoc (EditableInt) [EditableString] deriving (Show, Data, Typeable)

newtype Id = Id Int deriving (Show, Eq, Ord, Data, Typeable)
noId = Id (-1)

data EditableString = EditableString {getStrId :: Id, getStrVal :: String} deriving (Show, Data, Typeable)
data EditableInt = EditableInt {getIntId :: Id, getIntVal :: Int} deriving (Show, Data, Typeable)

{-
testDoc = TestDoc (EditableInt noId 2) 
            [EditableString noId "Martijn",EditableString noId "Tommy",EditableString noId "Pino"]

presentTestDoc (TestDoc i strs) =
  mkDiv "root" $
        presentRadioBox ["een", "twee", "drie"] i
    +++ presentTextfield (strs!!getIntVal i)
-}

presentRadioBox :: [String] -> EditableInt -> [Html]
presentRadioBox items (EditableInt (Id id) i) = radioBox id items i

presentTextfield :: EditableString -> Html
presentTextfield (EditableString (Id id) str) =
  textfield "" ! [identifier (show id), strAttr "VALUE" str
                 , strAttr "onChange" $ "textFieldChanged('"++show id++"')"]
{-
presentRoot (Root vet) = htmlPage "Piglet 2.0" $
  presentVet vet

presentVet (Vet name visits) =
      h1 << name 
  +++ map presentVisit visits

-- presentVisit :: Visit -> [Html] 
presentVisit (Visit zipCode date sties) = 
        h2 << ("Visit at "++zipCode ++" on " ++date)
    +++ map presentSty (zip [0..] sties)

presentSty (i,Sty pigs) =
        h3 << ("Sty nr "++show i)
    +++ map presentPig (zip [0..] pigs)

presentPig (i, Pig symptoms diagnosis) = p <<
  ("Pig nr. "++show i++"Symptoms: "+++presentSymptoms symptoms+++" diagnosis "++show diagnosis)

presentSymptoms [a,b,c] = 
       p << "Type varken: " 
   +++ p << "Fase cyclus: "
   +++ p << "Haren overeind: "
   +++ radioBox "q1" ["Ja", "Nee"] 0
-}
radioBox id items selectedIx =
  [ radio (show id) (show i) ! ( [strAttr "onChange" ("debugAdd('boing');queueCommand('Set("++show id++","++show i++");')") ]
                          ++ if i == selectedIx then [strAttr "checked" ""] else []) 
                          +++ item +++ br 
                        | (i, item) <- zip [0..] items ]


htmlPage title bdy = 
  thehtml $ concatHtml [ header $ thetitle $ toHtml title
                       , body bdy 
                       ]



-- Generics


-- number all Id's in the argument data structure uniquely
assignIds :: Data d => d -> d
assignIds x = snd $ everywhereAccum assignId 0 x

assignId :: Data d => Int -> d -> (Int,d)
assignId = mkAccT $ \i (Id _) -> (i+1, Id i)


type Updates = Map Id String  -- maps id's to the string representation of the new value

-- update the datastructure at the id's in Updates 
replace :: Data d => Updates -> d -> d
replace updates = everywhere $ extT (mkT (replaceEditableString updates))  (replaceEditableInt updates)

replaceEditableString :: Updates -> EditableString -> EditableString
replaceEditableString updates x@(EditableString i _) =
  case Map.lookup i updates of
    Just str -> (EditableString i str)
    Nothing -> x

replaceEditableInt :: Updates -> EditableInt -> EditableInt
replaceEditableInt updates x@(EditableInt i _) =
  case Map.lookup i updates of
    Just str -> (EditableInt i (read str))
    Nothing -> x


everywhereAccum :: Data d => (forall b . Data b => a -> b -> (a,b)) -> a -> d -> (a,d)
everywhereAccum f acc a =
 let (acc'',r) = gfoldl k z a
 in  f acc'' r  
 where k (acc',a2b) a = let (acc'',a') = everywhereAccum f acc' a
                            b = a2b a'
                        in  (acc'', b)
       z e = (acc, e)

gReplace :: forall a d . (Typeable a, Data d) => [a] -> d -> ([a],d)
gReplace = mkAccT $ \(i:is) _ -> (is,i)

number :: Data d => Int -> d -> (Int,d)
number  = mkAccT $ \acc _ -> (acc+1, acc)

mkAccT :: forall a b acc . (Typeable acc, Typeable a, Data b) => (acc -> a -> (acc,a)) -> (acc -> b -> (acc,b))
mkAccT f = case cast f  of -- can we do this without requiring Typeable acc?
                  Just g  -> g 
                  Nothing -> \acc b -> (acc,b)


-- classes
{-
class Presentable v where
  present :: v -> Html

class Storable v where
  store :: v -> (Data -> Data)

-}

class Presentable v where
  present :: v -> Html

class Presentable v => WebView v where
  load :: Database -> v
  save :: v -> (Database -> Database)


-- instead of explicitly declaring views, we first try to make the database type instance of WebView

instance Presentable Visit where
  present visit = htmlPage "Piglet 2.0" << "bla"

{- presentRoot (Root vet) = htmlPage "Piglet 2.0" $
  presentVet vet

presentVet (Vet name visits) =
      h1 << name 
  +++ map presentVisit visits

-- presentVisit :: Visit -> [Html] 
presentVisit (Visit zipCode date sties) = 
        h2 << ("Visit at "++zipCode ++" on " ++date)
    +++ map presentSty (zip [0..] sties)

presentSty (i,Sty pigs) =
        h3 << ("Sty nr "++show i)
    +++ map presentPig (zip [0..] pigs)

presentPig (i, Pig symptoms diagnosis) = p <<
  ("Pig nr. "++show i++"Symptoms: "+++presentSymptoms symptoms+++" diagnosis "++show diagnosis)
-}