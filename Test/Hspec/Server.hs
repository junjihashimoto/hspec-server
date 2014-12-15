
module Test.Hspec.Server where

import System.Process
import System.Exit
import System.IO
import qualified Test.Hspec.Core.Spec as Hspec
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Trans.Writer
--import Control.Monad.IO.Class
import qualified Shelly as Sh
import System.IO.Temp


type ServerName = String

class ServerType a where
  name :: a -> ServerName
  cmd :: a -> FilePath -> [String] -> String -> IO (ExitCode,String,String)

data Localhost

data Ssh = Ssh {
  sshHostName :: String
, sshId :: Maybe String
, sshConf :: Maybe String
, sshPort :: Maybe Int
} deriving (Show,Eq)

data Vagrant = Vagrant {
  vHostName :: String
} deriving (Show,Eq)

instance ServerType Localhost where
  name _ = "localhost"
  cmd _ = readProcessWithExitCode

-- instance ServerType Ssh where
--   name = vHostName
--   cmd d c arg i = readProcessWithExitCode "ssh" ((catMaybes ["-F",file]) ++ [c] ++ arg) i

instance ServerType Vagrant where
  name = vHostName
  cmd d c arg i = withSystemTempFile "hspec-server" $ \file handle -> do
    (_,conf,_) <- readProcessWithExitCode "vagrant" ["ssh-config"] []
    hPutStr handle conf
    readProcessWithExitCode "ssh" (["-F",file,c]++arg) i


data ServerExampleData dat = ServerExampleData
    { serverData :: !dat
    }

type ServerExample dat = ST.StateT (ServerExampleData dat) IO

type ServerSpec dat = Writer [ServerSpecTree dat] ()

data ServerSpecTree dat
    = ServerSpecGroup String [ServerSpecTree dat]
    | ServerSpecItem String (ServerExample dat ())

getServerData :: ServerExample dat dat
getServerData = fmap serverData ST.get

sdescribe :: String -> ServerSpec dat -> ServerSpec dat
sdescribe label sspecs = tell [ServerSpecGroup label $ execWriter sspecs]

serverSpec :: ServerType dat
           => dat
           -> ServerSpec dat
           -> Hspec.Spec
serverSpec serverData' sspecs =
    Hspec.fromSpecList $ map unServer $ execWriter sspecs
  where
    unServer (ServerSpecGroup x y) = Hspec.specGroup x $ map unServer y
    unServer (ServerSpecItem x y) = Hspec.specItem x $ 
        ST.evalStateT y ServerExampleData
            { serverData = serverData'
            }

sit :: String -> ServerExample dat () -> ServerSpec dat
sit label example = tell [ServerSpecItem label example]

