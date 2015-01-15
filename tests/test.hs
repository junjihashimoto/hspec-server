import Data.Monoid
import Test.Hspec
import Test.Hspec.Server
import Test.Hspec.Contrib.Retry
import Control.Monad.IO.Class

main :: IO ()
main = hspec $ do
    describe "hoge" $ with localhost $ do
      it "package test" $ do
        os <- getServerOS
        liftIO $ os `shouldBe` (Just $ Ubuntu "12.04")
      it "package test" $ do
        package "zookeeper" @>= installed
      it "port test" $ do
        port 2181 @>= listening
      it "service test" $ do
        service "zookeeper" @>= running
      it "command test" $ do
        command "echo" ["hoge"] [] @>= exit 0 <> stdout "hoge\n"
        v <- command "echo" ["hoge"] []
        v `includes'` (exit 0 <> stdout "hoge\n")
        liftIO $ getStdout v `shouldBe` Just "hoge\n"
        liftIO $ getStderr v `shouldBe` Just ""
      it "retry test" $ do
        retryWith 10 $
          command "ls" [] [] @>= exit 0
      it "network" $ do
        host "localhost" @>= reachable
        host "localhost" @== reachable
        hostWithPort "localhost" 2181 @>= reachable
