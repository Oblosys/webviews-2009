
module Database where
import Data.Map (Map)
import qualified Data.Map as Map

type QuestionTag = String

type Answer = String

type Database = Map QuestionTag Answer

mkInitialDatabase :: IO Database
mkInitialDatabase = return $ Map.empty

setAnswer :: QuestionTag -> Answer -> Database -> Database
setAnswer questionTag answer = Map.insert questionTag answer 
