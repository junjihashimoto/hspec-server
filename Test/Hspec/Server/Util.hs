module Test.Hspec.Server.Util where

import System.Exit
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Text.Regex.Posix
import Test.Hspec.Server.Core

cmd :: ServerType dat => dat -> FilePath -> [String] -> String -> IO (ExitCode,String,String)
cmd = stCmd

type Patterns = [String]
type Arg = [String]
type Input = String
type TestType = String
--type TestedName = String

cmdAndChk :: (ServerType dat)
          => TestType
          -> s
          -> s
          -> FilePath 
          -> Arg
          -> Input
          -> Patterns
          -> ServerExample dat (Either String s)
cmdAndChk test testedval _failedval c arg i [] = do
  dat <- getServerData
  c'@(code,_out,_) <- liftIO $ cmd dat c arg i
  if (code /= ExitSuccess) 
    then return $ Left $ test <> " error:" ++ show c'
    else return $ Right testedval
cmdAndChk test testedval failedval c arg i patterns = do
  dat <- getServerData
  (code,out,_) <- liftIO $ cmd dat c arg i
  if (code /= ExitSuccess) 
    then do 
      return $ Left $ test <> " error:" ++ show c
    else do
      if or (map (\v -> foldr (||) False 
                               (map (\p -> v =~ p) patterns)
                  )
                 (lines out))
        then return $ Right testedval
        else return $ Right failedval
