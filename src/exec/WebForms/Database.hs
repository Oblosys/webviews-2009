
module Database where
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import ObloUtils

type QuestionTag = String

type Answer = String

type Database = Map QuestionTag (Maybe (Bool, Answer))
-- TODO: use Answered here. We need a version of answered with the strings in it, so we don't have a string for Unanswered

mkInitialDatabase :: IO Database
mkInitialDatabase = return $ Map.empty

setAnswer :: QuestionTag -> (Maybe (Answer -> Bool)) -> Answer -> Database -> Database
setAnswer questionTag mValidate answer = Map.insert questionTag $ Just (valid, answer) 
 where valid = maybe True (\validate -> validate answer) mValidate

getAnswer :: QuestionTag -> Database -> Maybe String
getAnswer questionTag db =
  case unsafeLookup "getAnswer" questionTag db of -- don't need to do unsafe lookup, but it will help us find bugs
    Nothing      -> Nothing
    Just (_,str) -> Just str

clearAnswer :: QuestionTag -> Database -> Database
clearAnswer questionTag = Map.insert questionTag Nothing 

isQuestionAnswered :: (Maybe (Bool, Answer)) -> Bool
isQuestionAnswered Nothing          = False
isQuestionAnswered (Just (False,_)) = False
isQuestionAnswered _                = True