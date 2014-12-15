import Test.Hspec
import Test.Hspec.Server

main :: IO ()
main = hspec $ do
  serverSpec localhost $ do
    sdescribe "hoge" $ do
      sit "package test" $ do
        package "zookeeper" `includes` Installed
      sit "port test" $ do
        port 2181 `includes` Listening
      sit "service test" $ do
        service "cron" `includes` Running
        service "atd" `includes` Running
      sit "command test" $ do
        command "ls" [] [] `includes` Exit 0
