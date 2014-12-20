
module Test.Hspec.Server.Core where

import qualified Test.Hspec as Hspec
import qualified Test.HUnit as HUnit
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Test.Hspec.Server.Type

getServerData :: ServerType dat => ServerExample dat dat
getServerData = ask

getServerOS :: ServerType dat => ServerExample dat (Maybe ServerOS)
getServerOS = do 
  d <- ask
  return $ stOS d

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

(@>=) :: (ServerType dat, Sets s) => ServerExample dat s -> s -> ServerExample dat ()
(@>=) = includes
infix 1 @>= 

(@==) :: (ServerType dat, Sets s) => ServerExample dat s -> s -> ServerExample dat ()
(@==) org' ex = do
  org <- org'
  liftIO $ Hspec.shouldBe org ex
infix 1 @== 
