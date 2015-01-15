module Test.Hspec.Server.ServerStatus (
  ServerStatus
, installed
, running
, enabled
, listening
, package
, process
, service
, port
) where

import System.Exit
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Text.Regex.Posix
import Test.Hspec.Server.Core
import Test.Hspec.Server.Util
import qualified Data.Set as S

type ServerStatus = S.Set ServerStatus'
data ServerStatus' =
   Installed
 | Enabled
 | Running
 | Listening
   deriving (Show,Ord,Eq)

installed :: S.Set ServerStatus'
installed = S.singleton Installed
running :: S.Set ServerStatus'
running = S.singleton Running
enabled :: S.Set ServerStatus'
enabled = S.singleton Enabled
listening :: S.Set ServerStatus'
listening = S.singleton Listening


package :: ServerType dat => String -> ServerExample dat ServerStatus
package pkg = do
  r <- cmdAndChk "package" installed none "dpkg" ["-l",pkg] [] ["^ii +"<>pkg<>" "]
  case r of
    Left err -> error err
    Right v -> return v

process :: ServerType dat => String -> ServerExample dat ServerStatus
process ps = do
  dat <- getServerData
  (code,_,_) <- liftIO $ cmd dat "pgrep" [ps] []
  if code == ExitSuccess
    then return running
    else return none

service :: ServerType dat => String -> ServerExample dat ServerStatus
service s = do
  dat <- getServerData
  (_code,_out,_) <- liftIO $ cmd dat ("/etc/init.d/"++s) ["status"] []
  if _code == ExitSuccess 
    then check _out
    else do
      c@(code,out,_) <- liftIO $ cmd dat "sudo" ["service",s,"status"] []
      when (code /= ExitSuccess) $ do
        error $ "service error:" ++ show c
      check out
  where
    check out = 
      if or (map (\v ->
                   (v  =~ ("is running" :: String)) ||
                   (v  =~ ("start/running" :: String))
                 ) (lines out))
        then return running
        else return none

port :: ServerType dat => Int -> ServerExample dat ServerStatus
port p = do
  r <- cmdAndChk "port" listening none
           "netstat" ["-tanp"] [] 
           ["^tcp[ 6] +[0-9]+ +[0-9]+ :::" <> (show p) <> " +[^ ]+ +LISTEN",
            "^tcp[ 6] +[0-9]+ +[0-9]+ [0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+:" <> (show p) <> " +[^ ]+ +LISTEN"]
  case r of
    Left err -> error err
    Right v -> return v

