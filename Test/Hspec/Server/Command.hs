module Test.Hspec.Server.Command where

import System.Exit
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Text.Regex.Posix
import Test.Hspec.Server.Type
import Test.Hspec.Server.Core

package :: ServerType dat => String -> ServerExample dat ServerStatus
package pkg = do
  dat <- getServerData
  c@(code,out,_) <- liftIO $ cmd dat "dpkg" ["-l",pkg] []
  when (code /= ExitSuccess) $ do
    error $ "pacakge error:" ++ show c
  if or (map (\v -> v  =~ ("^ii +" <> pkg <> " ")) (lines out))
    then return Installed
    else return None

process :: ServerType dat => String -> ServerExample dat ServerStatus
process ps = do
  dat <- getServerData
  (code,_,_) <- liftIO $ cmd dat "pgrep" [ps] []
  if code == ExitSuccess
    then return Running
    else return None

service :: ServerType dat => String -> ServerExample dat ServerStatus
service s = do
  dat <- getServerData
  c@(code,out,_) <- liftIO $ cmd dat ("/etc/init.d/"++s) ["status"] []
  when (code /= ExitSuccess) $ do
    error $ "service error:" ++ show c
  if or (map (\v ->
               (v  =~ ("is running" :: String)) ||
               (v  =~ ("start/running" :: String))
             ) (lines out))
    then return Running
    else return None

port :: ServerType dat => Int -> ServerExample dat ServerStatus
port p = do
  dat <- getServerData
  c@(code,out,_) <- liftIO $ cmd dat "netstat" ["-tanp"] []
  when (code /= ExitSuccess) $ do
    error $ "port error:" ++ show c
  if or (map (\v ->
               (v  =~ ("^tcp[ 6] +[0-9]+ +[0-9]+ :::" <> show p <> " +[^ ]+ +LISTEN")) ||
               (v  =~ ("^tcp[ 6] +[0-9]+ +[0-9]+ [0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+:" <> show p <> " +[^ ]+ +LISTEN"))
             ) (lines out))
    then return Listening
    else return None

command :: ServerType dat => FilePath -> [String] -> String -> ServerExample dat CommandStatus
command c arg inp = do
  dat <- getServerData
  (code,out,err) <- liftIO $ cmd dat c arg inp
  let genCode ExitSuccess =  0
      genCode (ExitFailure val) =  val
  return $ Exit (genCode code) <> Stdout out <> Stderr err
