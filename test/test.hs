import Test.Hspec hiding (it,describe)
import Test.Hspec.Server
-- import Test.Hspec.Contrib.Retry
import Control.Monad.IO.Class

main :: IO ()
main = hspec $ do
  serverSpec localhost $ do
    describe "hoge" $ do
      it "package test" $ do
        dat <- getServerData
        os <- liftIO $ detectOS dat
        liftIO $ os `shouldBe` (Just $ Ubuntu "14.04")
      it "package test" $ do
        package "zookeeper" `includes` Installed
      it "port test" $ do
        port 2181 `includes` Listening
      it "service test" $ do
        service "cron" `includes` Running
        service "atd" `includes` Running
      it "command test" $ do
        command "ls" [] [] `includes` Exit 0
      -- it "retry test" $ do
      --   retryWith 10 $ --currently does not support Retry. I want to use retryWith !!!
      --     command "ls" [] [] `includes` Exit 0
