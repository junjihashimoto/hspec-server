
module Test.Hspec.Server.Core where

import qualified Test.Hspec.Core.Spec as Hspec
import qualified Test.HUnit as HUnit
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST
import Test.Hspec.Server.Type

serverSpecOS :: ServerType dat
           => dat
           -> ServerOS
           -> ServerSpec dat
           -> Hspec.Spec
serverSpecOS serverData' serverOS' sspecs = do
    dat' <- Hspec.runIO $ setup serverData'
    os' <- case serverOS' of
             AutoDetect -> do
               v <- Hspec.runIO $ detectOS dat'
               case v of
                 Just v' -> return v'
                 Nothing -> error "can not detect os"
             _ -> return serverOS'
    Hspec.fromSpecList $ map (unServer (dat',os')) $ execWriter sspecs
  where
    unServer (dat',os') (ServerSpecGroup x y) = Hspec.specGroup x $ map (unServer (dat',os')) y
    unServer (dat',os') (ServerSpecItem x y) = Hspec.specItem x $ 
        ST.evalStateT y ServerExampleData {
            serverData = dat'
          , serverOS = os'
         }

serverSpec :: ServerType dat
           => dat
           -> ServerSpec dat
           -> Hspec.Spec
serverSpec serverData' sspecs = do
  serverSpecOS serverData' AutoDetect sspecs

getServerData :: ServerType dat => ServerExample dat dat
getServerData = fmap serverData ST.get

getServerOS :: ServerExample dat ServerOS
getServerOS = fmap serverOS ST.get

it :: ServerType dat => String -> ServerExample dat () -> ServerSpec dat
it label example = tell [ServerSpecItem label example]

describe :: ServerType dat => String -> ServerSpec dat -> ServerSpec dat
describe label sspecs = tell [ServerSpecGroup label $ execWriter sspecs]

includes' :: (ServerType dat, Sets s) => s -> s -> ServerExample dat ()
includes' org ex =
  liftIO $ flip HUnit.assertBool (include org ex) $ concat
    [ "Expected status was ", show ex
    , " but received status was ", show org
    ]

includes :: (ServerType dat, Sets s) => ServerExample dat s -> s -> ServerExample dat ()
includes org' ex = do
  org <- org'
  org `includes'` ex

