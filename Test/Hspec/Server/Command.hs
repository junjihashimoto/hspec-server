module Test.Hspec.Server.Command (
  package
, process
, service
, port
, command
  )where

import System.Exit
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Text.Regex.Posix
import Test.Hspec.Server.Type
import Test.Hspec.Server.Core

cmd :: ServerType dat => dat -> FilePath -> [String] -> String -> IO (ExitCode,String,String)
cmd = stCmd

type Patterns = [String]
type Arg = [String]
type Input = String
type TestType = String
type TestedName = String

cmdAndChk :: (ServerType dat, Sets s)
          => TestType
          -> s
          -> s
          -> FilePath 
          -> Arg
          -> Input
          -> Patterns
          -> ServerExample dat (Either String s)
cmdAndChk test testedval failedval c arg i [] = do
  dat <- getServerData
  c@(code,out,_) <- liftIO $ cmd dat c arg i
  if (code /= ExitSuccess) 
    then return $ Left $ test <> " error:" ++ show c
    else return $ Right testedval
cmdAndChk test testedval failedval c arg i patterns = do
  dat <- getServerData
  c@(code,out,_) <- liftIO $ cmd dat c arg i
  if (code /= ExitSuccess) 
    then do 
      return $ Left $ test <> " error:" ++ show c
    else do
      if or (map (\v -> foldr (||) False 
                               (map (\p -> v =~ p) patterns)
                  )
                 (lines out))
        then return $ Right testedval
        else return $ Right failedval

package :: ServerType dat => String -> ServerExample dat ServerStatus
package pkg = do
  r <- cmdAndChk "package" Installed None "dpkg" ["-l",pkg] [] ["^ii +"<>pkg<>" "]
  case r of
    Left err -> error err
    Right v -> return v

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
        then return Running
        else return None

port :: ServerType dat => Int -> ServerExample dat ServerStatus
port p = do
  r <- cmdAndChk "port"  Listening None 
           "netstat" ["-tanp"] [] 
           ["^tcp[ 6] +[0-9]+ +[0-9]+ :::" <> (show p) <> " +[^ ]+ +LISTEN",
            "^tcp[ 6] +[0-9]+ +[0-9]+ [0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+:" <> (show p) <> " +[^ ]+ +LISTEN"]
  case r of
    Left err -> error err
    Right v -> return v

command :: ServerType dat => FilePath -> [String] -> String -> ServerExample dat CommandStatus
command c arg inp = do
  dat <- getServerData
  (code,out,err) <- liftIO $ cmd dat c arg inp
  let genCode ExitSuccess =  0
      genCode (ExitFailure val) =  val
  return $ Exit (genCode code) <> Stdout out <> Stderr err
