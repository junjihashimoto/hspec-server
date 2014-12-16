{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}
module Test.Hspec.Server.Type where

import System.Exit
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Trans.Writer

import Data.Monoid
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

-- instance (ServerType dat) => Hspec.Example (ServerExample dat ()) where
--   type Arg (ServerExample dat ()) = ()
--   evaluateExample e = Hspec.evaluateExample (\() -> e)

-- instance Hspec.Example (a -> (ServerExample dat ())) where
--     type Arg (a -> (ServerExample dat ())) = a
--     evaluateExample e _ action _ = (action e >> return Hspec.Success) `E.catches` [
--         E.Handler (\(HUnit.HUnitFailure err) -> return (Hspec.Fail err))
--       , E.Handler (return :: Hspec.Result -> IO Hspec.Result)
      -- ]

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
