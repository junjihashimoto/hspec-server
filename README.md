# Hspec-Server: Test Framework like ServerSpec for Haskell

[![Build Status](https://travis-ci.org/junjihashimoto/hspec-server.png?branch=master)](https://travis-ci.org/junjihashimoto/hspec-server) [![Coverage Status](https://coveralls.io/repos/junjihashimoto/hspec-server/badge.png)](https://coveralls.io/r/junjihashimoto/hspec-server)

Hspec-Server is test framework for checking server's status.
It is inspired by the Ruby library ServerSpec.

## Getting started

Install this from Hackage.

    cabal update && cabal install hspec-server

## Usage

Put "with(ServerType)" after hspec's describe-sentence.
Currently localhost, ssh and vagrant are supported for ServerType.
Examples are below. "@>=" is like hspec's shouldBe.
it can check multiple values.

```
    describe "test for localhost" $ with localhost $ do
      it "package zookeepr" $ do
        package "zookeeper" @>= Installed
      it "port test" $ do
        port 2181 @>= Listening
      it "service test" $ do
        service "cron" @>= Running
      it "command test" $ do
        command "echo" ["hoge"] [] @>=  Exit 0 <> Stdout "hoge\n"
```
