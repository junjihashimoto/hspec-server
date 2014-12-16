{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}
module Test.Hspec.Server.Type where

import System.Exit
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Trans.Writer
import qualified Test.Hspec.Core.Spec as Hspec

import Data.Monoid
import Data.List
import Data.Maybe
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
  setup :: a -> IO a
  name :: a -> ServerName
  cmd :: a -> FilePath -> [String] -> String -> IO (ExitCode,String,String)

data ServerExampleData dat = ServerExampleData {
    serverData :: !dat
  , serverOS :: !ServerOS
  }

type ServerExample dat = ST.StateT (ServerExampleData dat) IO

instance (ServerType dat) => Hspec.Example (ServerExample dat a) where
  type Arg (ServerExample dat a) = dat
  evaluateExample example params action =
    Hspec.evaluateExample
      (action $ \dat' -> do
        os <- detectOS dat'
        _ <- ST.evalStateT example ServerExampleData {
            serverData = dat'
          , serverOS = fromJust os
          }
        return ())
      params
      ($ ())

type ServerSpec dat = Writer [ServerSpecTree dat] ()

data ServerSpecTree dat
    = ServerSpecGroup String [ServerSpecTree dat]
    | ServerSpecItem String (ServerExample dat ())

class Show a => Sets a where
  include :: a -> a -> Bool

data ServerStatus =
   SAnd (S.Set ServerStatus)
 | Installed
 | Enabled
 | Running
 | Listening
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

data CommandStatus =
   CAnd (S.Set CommandStatus)
 | Exit Int
 | Stdout String
 | Stderr String
 | CNone
   deriving (Show,Ord,Eq)

instance Monoid CommandStatus where
  mempty = CNone
  mappend CNone a = a
  mappend a CNone = a
  mappend (CAnd a) (CAnd b) = CAnd (a<>b)
  mappend (CAnd a) b = CAnd (a <> S.singleton b)
  mappend a (CAnd b) = CAnd (S.singleton a <> b)
  mappend a b = CAnd (S.singleton a <> S.singleton b)

instance Sets ServerStatus where
  include (SAnd org') (SAnd exp') = flip S.isSubsetOf org' exp'
  include org' (SAnd exp') = flip S.isSubsetOf (S.singleton org') exp'
  include (SAnd org') exp' = flip S.isSubsetOf org' (S.singleton exp')
  include org' exp' = flip S.isSubsetOf (S.singleton org') (S.singleton exp')

instance Sets CommandStatus where
  include (CAnd org') (CAnd exp') = flip S.isSubsetOf org' exp'
  include org' (CAnd exp') = flip S.isSubsetOf (S.singleton org') exp'
  include (CAnd org') exp' = flip S.isSubsetOf org' (S.singleton exp')
  include org' exp' = flip S.isSubsetOf (S.singleton org') (S.singleton exp')

getStdout :: CommandStatus -> Maybe String
getStdout (Stdout code) = Just code
getStdout (CAnd statuss) = listToMaybe $ catMaybes $ map getStdout $ S.toList statuss
getStdout _ = Nothing

getStderr :: CommandStatus -> Maybe String
getStderr (Stderr code) = Just code
getStderr (CAnd statuss) = listToMaybe $ catMaybes $ map getStdout $ S.toList statuss
getStderr _ = Nothing


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
