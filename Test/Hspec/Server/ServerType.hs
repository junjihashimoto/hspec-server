module Test.Hspec.Server.ServerType where

import System.Process
import System.Exit
import System.IO.Temp
import System.IO
import Data.Monoid
import Data.Maybe
import Control.Monad
import Test.Hspec.Server.Type

data Localhost = Localhost deriving (Show,Eq)

localhost :: Localhost
localhost = Localhost

instance ServerType Localhost where
  setup a = return a
  name _ = "localhost"
  cmd _ = readProcessWithExitCode

data Ssh = Ssh {
  sshHostName :: String
, sshId :: Maybe String
, sshConf :: Maybe String
, sshPort :: Maybe Int
, sshUser :: Maybe String
} deriving (Show,Eq)

ssh :: String -> Ssh
ssh hostname = Ssh hostname Nothing Nothing Nothing Nothing

instance ServerType Ssh where
  setup a = return a
  name = sshHostName
  cmd d c arg i = do
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
} deriving (Show,Eq)

vagrant :: String -> Vagrant
vagrant hostname = Vagrant hostname Nothing

instance ServerType Vagrant where
  setup a = do
    (e,conf,_) <- readProcessWithExitCode "vagrant" ["ssh-config"] []
    when (e /= ExitSuccess) $ do
      error "vagrant setup error"
    return $ a {vConf = Just conf}
  name = vHostName
  cmd d c arg i = withSystemTempFile "hspec-server" $ \file handle -> do
    hPutStr handle (fromJust (vConf d))
    hClose handle
    readProcessWithExitCode "ssh" (["-F",file,name d,c]++arg) i

