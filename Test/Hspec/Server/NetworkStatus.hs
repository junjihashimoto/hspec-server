module Test.Hspec.Server.NetworkStatus (
  NetworkStatus
, reachable
, host
, hostWithPort
) where

import System.Exit
import Control.Monad.IO.Class
import Test.Hspec.Server.Core
import Test.Hspec.Server.Util
import qualified Data.Set as S

type NetworkStatus = S.Set NetworkStatus'
data NetworkStatus' =
   Reachable
   deriving (Show,Ord,Eq)

reachable :: S.Set NetworkStatus'
reachable = S.singleton Reachable

host :: ServerType dat => String -> ServerExample dat NetworkStatus
host hostname = do
  dat <- getServerData
  (code,_,_) <- liftIO $ cmd dat "ping" ["-c","1",hostname] []
  if code == ExitSuccess
    then return reachable
    else return none

hostWithPort :: ServerType dat => String -> Int -> ServerExample dat NetworkStatus
hostWithPort hostname port = do
  dat <- getServerData
  (code,_,_) <- liftIO $ cmd dat "nc" [hostname,show port] []
  if code == ExitSuccess
    then return reachable
    else return none
