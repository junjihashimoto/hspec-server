
module Test.Hspec.Server.Core where

import System.Exit
import qualified Test.Hspec.Core.Spec as Hspec
import qualified Test.HUnit as HUnit
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST
import Data.List
import Data.Maybe
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

detectOS :: ServerType dat => dat -> IO (Maybe ServerOS)
detectOS dat = do
  (_,out,_) <- cmd dat "sh" ["-c","echo $OSTYPE"] []
  case (head (lines out)) of
    "linux-gnu" -> detectLinux dat
    'd':'a':'r':'w':'i':'n':o -> return $ Just $ MacOS o
    "msys" -> return $ Just $ Windows "msys"
    "cygwin" -> return $ Just $ Windows "cygwin"
    "win32" -> return $ Just $ Windows "win32"
    "win64" -> return $ Just $ Windows "win64"
    'f':'r':'e':'e':'b':'s':'d':o -> return $ Just $ FreeBSD o
    o -> return $ Just $ OtherOS o

detectLinux :: ServerType dat => dat -> IO (Maybe ServerOS)
detectLinux dat = do
  (_code,_out,_) <- cmd dat "cat" ["/etc/lsb-release"] []
  if _code == ExitSuccess
    then do
      let tag = "DISTRIB_RELEASE="
      let v = listToMaybe $ map (drop (length tag)) $ filter (isPrefixOf "DISTRIB_RELEASE=") (lines _out)
      case v of
        Just v' -> return $ Just $ Ubuntu v'
        Nothing -> return $ Just $ Ubuntu ""
    else do
      (_code,_out,_) <- cmd dat "cat" ["/etc/debian_version"] []
      if _code == ExitSuccess
        then return $ Just $ Debian _out
        else do
          (_code,_out,_) <- cmd dat "cat" ["/etc/centos-release"] []
          if _code == ExitSuccess
            then return $ Just $ CentOS _out
            else do
              (_code,_out,_) <- cmd dat "cat" ["/etc/fedora-release"] []
              if _code == ExitSuccess
                then return $ Just $ Fedora _out
                else do
                  (_code,_out,_) <- cmd dat "cat" ["/etc/redhat-release"] []
                  if _code == ExitSuccess
                    then return $ Just $ Fedora _out
                    else return $ Just $ LinuxOther ""
