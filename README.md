proctest
========

A Haskell library for testing interactive command line programs.

With Proctest you can write beautiful tests, for example testing `cat` with [hspec](http://hspec.github.com):

```haskell
import Control.Applicative
import Test.Hspec
import Test.HUnit
import Test.Proctest

main = hspec $ describe "cat" $ do
  it "prints out what we put in" $ do

    -- Start up the program to test
    (hIn, hOut, hErr, p) <- run "cat" []

    -- Make sure buffering doesn't prevent us from reading what we expect
    setBuffering NoBuffering [hIn, hOut]

    -- Communicate with the program
    hPutStrLn hIn "hello world"

    -- Define a convenient wrapper around 'waitOutput'.
    --
    -- It specifies how long we have to wait
    -- (malfunctioning programs shall not block automated testing for too long)
    -- and how many bytes we are sure the expected response fits into
    -- (malfunctioning programs shall not flood us with garbage either).
    let catWait h = asUtf8Str <$> waitOutput (seconds 0.01) 1000 h -- Wait max 10 ms, 1000 bytes

    -- Wait a little to allow `cat` processing the input
    sleep (seconds 0.00001)

    -- Read the response
    response <- catWait hOut

    -- Test if it is what we want (here using HUnit's 'expectEqual')
    response @?= "hello world\n"
```

Install
-------

[Proctest is on Hackage](http://hackage.haskell.org/package/proctest).

`cabal install proctest`
