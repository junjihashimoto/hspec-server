{-# LANGUAGE TypeFamilies, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}

module Test.Hspec.Server.Core where

import System.Exit
import Control.Monad.Trans.Reader
import qualified Test.Hspec.Core.Spec as Hspec
import Test.Hspec (beforeAll)

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as S

import qualified Test.Hspec as Hspec
import qualified Test.HUnit as HUnit
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Set as S

data ServerOS =
   Ubuntu String
 | Debian String
 | CentOS String
 | Fedora String
 | Redhat String
 | LinuxOther String
 | FreeBSD String
 | MacOS String
 | Windows String
 | OtherOS String
 | AutoDetect
   deriving (Show,Eq)

type ServerName = String

class ServerType a where
  stSetup :: a -> IO a
  stOS :: a -> Maybe ServerOS
  stName :: a -> ServerName
  stCmd :: a -> FilePath -> [String] -> String -> IO (ExitCode,String,String)

type ServerExample dat = ReaderT dat IO

with :: ServerType dat => dat -> Hspec.SpecWith dat -> Hspec.Spec
with d = beforeAll (stSetup d)

instance (ServerType dat) => Hspec.Example (ServerExample dat ()) where
  type Arg (ServerExample dat ()) = dat
  evaluateExample example params action =
    Hspec.evaluateExample
      (action $ runReaderT example)
      params
      ($ ())

include :: Ord a => S.Set a -> S.Set a -> Bool
include a b = S.isSubsetOf b a
none :: S.Set a
none = S.empty

detectOS :: ServerType dat => dat -> IO (Maybe ServerOS)
detectOS dat = do
  v@(code,out,_) <- stCmd dat "bash" ["-c","echo $OSTYPE"] []
  when (code /= ExitSuccess) $ do
    error $ "detectOS's error;" ++ show v
  case listToMaybe (lines out) of
    Just str -> checkEnv str
    Nothing -> return Nothing
  where
    checkEnv str =   
      case str of
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
  let cmd = stCmd
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

getServerData :: ServerType dat => ServerExample dat dat
getServerData = ask

getServerOS :: ServerType dat => ServerExample dat (Maybe ServerOS)
getServerOS = do 
  d <- ask
  return $ stOS d

includes' :: (ServerType dat,Show s,Ord s) => S.Set s -> S.Set s -> ServerExample dat ()
includes' org ex =
  liftIO $ flip HUnit.assertBool (include org ex) $ concat
    [ "Expected status was ", show ex
    , " but received status was ", show org
    ]

includes :: (ServerType dat,Show s,Ord s) => ServerExample dat (S.Set s) -> (S.Set s) -> ServerExample dat ()
includes org' ex = do
  org <- org'
  org `includes'` ex

(@>=) :: (ServerType dat,Show s,Ord s) => ServerExample dat (S.Set s) -> S.Set s -> ServerExample dat ()
(@>=) = includes
infix 1 @>= 

(@==) :: (ServerType dat,Show s,Ord s) => ServerExample dat (S.Set s) -> S.Set s -> ServerExample dat ()
(@==) org' ex = do
  org <- org'
  liftIO $ Hspec.shouldBe org ex
infix 1 @== 
