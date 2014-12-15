{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Server where

import System.Process
import System.Exit
import System.IO
import qualified Test.Hspec.Core.Spec as Hspec
import qualified Test.HUnit as HUnit
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class
import System.IO.Temp

import Data.Monoid
import qualified Data.Set as S
import Text.Regex.Posix


data ServerOS =
   Ubuntu String
 | Debian String
 | FreeBSD String
 | MacOS String
 | Windows String
 | OtherOS String
   deriving (Show,Eq)

type ServerName = String

class ServerType a where
  name :: a -> ServerName
  cmd :: a -> FilePath -> [String] -> String -> IO (ExitCode,String,String)

data Localhost = Localhost deriving (Show,Eq)

localhost :: Localhost
localhost = Localhost

data Ssh = Ssh {
  sshHostName :: String
, sshId :: Maybe String
, sshConf :: Maybe String
, sshPort :: Maybe Int
, sshUser :: Maybe String
} deriving (Show,Eq)

ssh :: String -> Ssh
ssh hostname = Ssh hostname Nothing Nothing Nothing Nothing

data Vagrant = Vagrant {
  vHostName :: String
} deriving (Show,Eq)

vagrant :: String -> Vagrant
vagrant hostname = Vagrant hostname

instance ServerType Localhost where
  name _ = "localhost"
  cmd _ = readProcessWithExitCode

instance ServerType Ssh where
  name = sshHostName
  cmd d c arg i = do
    readProcessWithExitCode "ssh" (sshOpt ++ [sshHost] ++ [c] ++ arg) i
    where
      sshOpt =
        (maybe [] (\v-> ["-p",show v]) (sshPort d)) ++
        (maybe [] (\v-> ["-i",show v]) (sshId d)) ++
        (maybe [] (\v-> ["-F",show v]) (sshConf d))
      sshHost = (maybe "" (\v -> v <> "@") (sshUser d)) <> sshHostName d

instance ServerType Vagrant where
  name = vHostName
  cmd d c arg i = withSystemTempFile "hspec-server" $ \file handle -> do
    (_,conf,_) <- readProcessWithExitCode "vagrant" ["ssh-config"] []
    hPutStr handle conf
    hClose handle
    readProcessWithExitCode "ssh" (["-F",file,name d,c]++arg) i

data ServerExampleData dat = ServerExampleData {
    serverData :: !dat
  , serverOS :: !ServerOS
  }

type ServerExample dat = ST.StateT (ServerExampleData dat) IO

type ServerSpec dat = Writer [ServerSpecTree dat] ()

data ServerSpecTree dat
    = ServerSpecGroup String [ServerSpecTree dat]
    | ServerSpecItem String (ServerExample dat ())

detectOS :: ServerType dat => dat -> IO (Maybe ServerOS)
detectOS _dat = return $ Just $ Ubuntu "precise"

getServerData :: ServerType dat => ServerExample dat dat
getServerData = fmap serverData ST.get

sdescribe :: ServerType dat => String -> ServerSpec dat -> ServerSpec dat
sdescribe label sspecs = tell [ServerSpecGroup label $ execWriter sspecs]

serverSpecOS :: ServerType dat
           => dat
           -> ServerOS
           -> ServerSpec dat
           -> Hspec.Spec
serverSpecOS serverData' serverOS' sspecs =
    Hspec.fromSpecList $ map unServer $ execWriter sspecs
  where
    unServer (ServerSpecGroup x y) = Hspec.specGroup x $ map unServer y
    unServer (ServerSpecItem x y) = Hspec.specItem x $ 
        ST.evalStateT y ServerExampleData {
            serverData = serverData'
          , serverOS = serverOS'
         }

serverSpec :: ServerType dat
           => dat
           -> ServerSpec dat
           -> Hspec.Spec
serverSpec serverData' sspecs = do
  serverOS' <- Hspec.runIO $ detectOS serverData'
  case serverOS' of
    Just os' -> serverSpecOS serverData' os' sspecs
    Nothing -> error "can not detect os"


sit :: ServerType dat => String -> ServerExample dat () -> ServerSpec dat
sit label example = tell [ServerSpecItem label example]

data ServerStatus =
   SAnd (S.Set ServerStatus)
 | Installed
 | Enabled
 | Running
 | Listening
 | Exit Int
 | Stdout String
 | Stderr String
 | None
   deriving (Show,Ord,Eq)

instance Monoid ServerStatus where
  mempty = None
  mappend None a = a
  mappend a None = a
  mappend (SAnd a) (SAnd b) = SAnd (a<>b)
  mappend (SAnd a) b = SAnd (a <> S.singleton b)
  mappend a (SAnd b) = SAnd (S.singleton a <> b)
  mappend a b = SAnd (S.singleton a <> S.singleton b)


includes' :: ServerType dat => ServerStatus -> ServerStatus -> ServerExample dat ()
includes' org ex =
  liftIO $ flip HUnit.assertBool (includes'' org ex) $ concat
    [ "Expected status was ", show ex
    , " but received status was ", show org
    ]
  where
    includes'' :: ServerStatus -> ServerStatus -> Bool
    includes'' (SAnd org') (SAnd exp') = flip S.isSubsetOf org' exp'
    includes'' org' (SAnd exp') = flip S.isSubsetOf (S.singleton org') exp'
    includes'' (SAnd org') exp' = flip S.isSubsetOf org' (S.singleton exp')
    includes'' org' exp' = flip S.isSubsetOf (S.singleton org') (S.singleton exp')

includes :: ServerType dat => ServerExample dat ServerStatus -> ServerStatus -> ServerExample dat ()
includes org' ex = do
  org <- org'
  org `includes'` ex

package :: ServerType dat => String -> ServerExample dat ServerStatus
package pkg = do
  dat <- getServerData
  (_,out,_) <- liftIO $ cmd dat "dpkg" ["-l",pkg] []
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
  (_,out,_) <- liftIO $ cmd dat ("/etc/init.d/"++s) ["status"] []
  if or (map (\v ->
               (v  =~ ("is running" :: String)) ||
               (v  =~ ("start/running" :: String))
             ) (lines out))
    then return Running
    else return None

port :: ServerType dat => Int -> ServerExample dat ServerStatus
port p = do
  dat <- getServerData
  (_,out,_) <- liftIO $ cmd dat "netstat" ["-tanp"] []
  if or (map (\v ->
               (v  =~ ("^tcp[ 6] +[0-9]+ +[0-9]+ :::" <> show p <> " +[^ ]+ +LISTEN")) ||
               (v  =~ ("^tcp[ 6] +[0-9]+ +[0-9]+ [0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+:" <> show p <> " +[^ ]+ +LISTEN"))
             ) (lines out))
    then return Listening
    else return None

command :: ServerType dat => FilePath -> [String] -> String -> ServerExample dat ServerStatus
command c arg inp = do
  dat <- getServerData
  (code,out,err) <- liftIO $ cmd dat c arg inp
  let genCode ExitSuccess =  0
      genCode (ExitFailure val) =  val
  return $ Exit (genCode code) <> Stdout out <> Stderr err
