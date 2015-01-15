module Test.Hspec.Server.CommandStatus (
  CommandStatus
, exit
, stdout
, stderr
, getStdout
, getStderr
, command
) where

import System.Exit
import Control.Monad.IO.Class
import Data.Monoid
import Test.Hspec.Server.Core
import Test.Hspec.Server.Util
import qualified Data.Set as S

type CommandStatus = S.Set CommandStatus'
data CommandStatus' =
   Exit Int
 | Stdout String
 | Stderr String
   deriving (Show,Ord,Eq)

exit :: Int -> S.Set CommandStatus'
exit n = S.singleton (Exit n)
stdout :: String -> S.Set CommandStatus'
stdout str = S.singleton (Stdout str)
stderr :: String -> S.Set CommandStatus'
stderr str = S.singleton (Stderr str)

getStdout :: CommandStatus -> Maybe String
getStdout stat = get (S.toList stat)
  where 
    get [] = Nothing
    get (x:xs) = 
      case x of
        Stdout str -> Just str
        _ -> get xs

getStderr :: CommandStatus -> Maybe String
getStderr stat = get (S.toList stat)
  where 
    get [] = Nothing
    get (x:xs) = 
      case x of
        Stderr str -> Just str
        _ -> get xs


command :: ServerType dat => FilePath -> [String] -> String -> ServerExample dat CommandStatus
command c arg inp = do
  dat <- getServerData
  (code,out,err) <- liftIO $ cmd dat c arg inp
  let genCode ExitSuccess =  0
      genCode (ExitFailure val) =  val
  return $ exit (genCode code) <> stdout out <> stderr err
