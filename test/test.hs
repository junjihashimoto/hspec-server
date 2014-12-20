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
        package "zookeeper" @>= Installed
      it "port test" $ do
        port 2181 @>= Listening
      it "service test" $ do
        service "zookeeper" @>= Running
      it "command test" $ do
        command "echo" ["hoge"] [] @>=  Exit 0 <> Stdout "hoge\n"
      it "retry test" $ do
        retryWith 10 $
          command "ls" [] [] @>= Exit 0
