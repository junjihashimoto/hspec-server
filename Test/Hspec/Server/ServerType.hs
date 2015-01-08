module Test.Hspec.Server.ServerType where

import System.Process
import System.Exit
import System.IO.Temp
import System.IO
import Data.Monoid
import Data.Maybe
import Control.Monad
import Test.Hspec.Server.Type

data Localhost = Localhost {
  lOS :: !(Maybe ServerOS)
  }deriving (Show,Eq)

localhost :: Localhost
localhost = Localhost Nothing

instance ServerType Localhost where
  stSetup a = do
    os' <- detectOS a
    return $ a {lOS = os'}
  stOS = lOS
  stName _ = "localhost"
  stCmd _ = readProcessWithExitCode

data Ssh = Ssh {
  sshHostName :: String
, sshId :: Maybe String
, sshConf :: Maybe String
, sshPort :: Maybe Int
, sshUser :: Maybe String
, sshOS :: !(Maybe ServerOS)
} deriving (Show,Eq)

ssh :: String -> Ssh
ssh hostname = Ssh hostname Nothing Nothing Nothing Nothing Nothing

instance ServerType Ssh where
  stSetup a = do
    os' <- detectOS a
    return $ a {sshOS = os'}
  stOS = sshOS
  stName = sshHostName
  stCmd d c arg i = do
    readProcessWithExitCode "ssh" (sshOpt ++ [sshHost] ++ [c] ++ arg) i
    where
      sshOpt =
        (maybe [] (\v-> ["-p",show v]) (sshPort d)) ++
        (maybe [] (\v-> ["-i",show v]) (sshId d)) ++
        (maybe [] (\v-> ["-F",show v]) (sshConf d))
      sshHost = (maybe "" (\v -> v <> "@") (sshUser d)) <> sshHostName d

data Vagrant = Vagrant {
   vHostName :: String
 , vConf :: Maybe String
 , vOS :: !(Maybe ServerOS)
} deriving (Show,Eq)

vagrant :: String -> Vagrant
vagrant hostname = Vagrant hostname Nothing Nothing

instance ServerType Vagrant where
  stSetup a = do
    (e,conf,_) <- readProcessWithExitCode "vagrant" ["ssh-config"] []
    when (e /= ExitSuccess) $ do
      error "vagrant setup error"
    os' <- detectOS (a {vConf = Just conf})
    return $ a {vConf = Just conf,vOS = os'}
  stOS = vOS
  stName = vHostName
  stCmd d c arg i = withSystemTempFile "hspec-server" $ \file handle -> do
    hPutStr handle (fromJust (vConf d))
    hClose handle
    readProcessWithExitCode "ssh" (["-F",file,stName d,c]++arg) i

data Docker = Docker {
   dContainerId :: String
 , dOS :: !(Maybe ServerOS)
} deriving (Show,Eq)

docker :: String ->  Docker
docker containerid = Docker containerid Nothing

instance ServerType Docker where
  stSetup a = do
    os' <- detectOS a
    return $ a {dOS = os'}
  stOS = dOS
  stName = dContainerId
  stCmd d c arg i = readProcessWithExitCode "docker" (["exec",stName d,c]++arg) i
