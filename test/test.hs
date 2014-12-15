
main = hspec $ do
  sdescribe (Vagrant "zh0") $ do
    sit "package test" $ do
      package "httpd" `shouldBe` Installed
      service "httpd" `shouldBe` Enabled <> Running
      port 80 `shouldBe` Listening
      command "ls -sl" `shouldBe` Stdout "" <> Exit 80 
  sdescribe (Vagrant "zh1") $ do
    sit "package test" $ do
      package "httpd" `shouldBe` Installed
      service "httpd" `shouldBe` Enabled <> Running
      port 80 `shouldBe` Listening
      command "ls -sl" `shouldBe` Stdout "" <> Exit 80 
  sdescribe (Vagrant "zh0") $ do
    sit "package test" $ do
      package "httpd" `shouldBe` Installed
      service "httpd" `shouldBe` Enabled <> Running
      port 80 `shouldBe` Listening
      command "ls -sl" `shouldBe` Stdout "" <> Exit 80 
  
