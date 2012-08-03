module Main where


import Control.Applicative
import Control.Concurrent (threadDelay)
import Test.Hspec
import Test.HUnit
import Test.Proctest


catTest = do
  (hIn, hOut, hErr, p) <- run "cat" []

  setBuffering LineBuffering [hIn, hOut]

  hPutStrLn hIn "test line 1"

  let catWait h = fmap asUtf8Str <$> waitOutputNoEx 10 1000 h -- Wait max 10 ms, 1000 bytes

  response <- catWait hOut

  putStrLn $ case response of
    Just x | x == "test line 1\n" -> "cat test successful"
    Just x                        -> "cat test failed. Output was: " ++ show x
    Nothing                       -> "cat timed out, producing no output"

  mExitCode <- getProcessExitCode p

  case mExitCode of
    Just ExitSuccess     -> putStrLn $ "process exited normally"
    Just (ExitFailure n) -> putStrLn $ "process quit with exit code " ++ show n
    Nothing              -> do putStrLn "process did not quit, killing it"
                               terminateProcess p



ncTest = hspec $ do
  describe "nc" $ do

    it "does a simple server <-> client interaction" $ do
      (serverIn, serverOut, serverErr, serverP) <- run "nc" ["-l", "1234"]
      (clientIn, clientOut, clientErr, clientP) <- run "nc" ["localhost", "1234"]

      -- Make sure processes are running
      serverExitCode <- getProcessExitCode serverP
      clientExitCode <- getProcessExitCode clientP

      setBuffering LineBuffering [clientIn, clientOut, serverIn, serverOut]

      return $ describe "after the connection has been set up" $ do

        it "the server is still running" $ serverExitCode @?= Nothing
        it "the client is still running" $ clientExitCode @?= Nothing

        let ncWait h = asUtf8Str <$> waitOutput 10 100 h

        it "server receives client request" $ do

          hPutStrLn clientIn "request 1"
          r <- ncWait serverOut
          r @?= "request 1\n"

        it "client receives server response" $ do

          hPutStrLn serverIn "response 1"
          r <- ncWait clientOut
          r @?= "response 1\n"

      hClose clientIn
      -- hClose serverIn
      -- hClose clientOut
      -- hClose serverOut

      terminateProcesses [serverP, clientP]

      serverExitCode <- getProcessExitCode serverP
      clientExitCode <- getProcessExitCode clientP
      return $ describe "after shutting down" $ do
        it "the server is still running" $ serverExitCode @?= Just ExitSuccess
        it "the client is still running" $ clientExitCode @?= Nothing



infix 1 ?@
(?@) :: (AssertionPredicable t) => String -> t -> Assertion
(?@) = flip (@?)


data EqualAssertion a = EqualAssertion a a

data LabeledAssertion a = LabeledAssertion String (EqualAssertion a)


(?==) :: (Eq a, Show a) => a -> a -> EqualAssertion a
actual ?== expected = EqualAssertion actual expected

label :: String -> EqualAssertion a -> LabeledAssertion a
label = LabeledAssertion


instance (Eq a, Show a) => Assertable (EqualAssertion a) where
  assert (EqualAssertion actual expected) = actual @?= expected

instance (Eq a, Show a) => Assertable (LabeledAssertion a) where
  assert (LabeledAssertion msg (EqualAssertion actual expected)) = assertEqual msg actual expected

assertLabel str equalsAssertion = assert (label str equalsAssertion)



ncTestHunit = it "does a simple server <-> client interaction (1)" $ do

  (serverIn, serverOut, serverErr, serverP) <- run "nc" ["-l", "1234"]
  (clientIn, clientOut, clientErr, clientP) <- run "nc" ["localhost", "1234"]

  -- Make sure processes are running
  serverExitCode <- getProcessExitCode serverP
  clientExitCode <- getProcessExitCode clientP

  assertLabel "server is running" $ serverExitCode ?== Nothing
  assertLabel "client is running" $ clientExitCode ?== Nothing

  setBuffering LineBuffering [clientIn, clientOut, serverIn, serverOut]

  let ncWait h = asUtf8Str <$> waitOutput 10 100 h

  hPutStrLn clientIn "request 1"
  r <- ncWait serverOut
  assertLabel "server receives client request" $ r ?== "request 1\n"

  hPutStrLn serverIn "response 1"
  r <- ncWait clientOut
  assertLabel "client receives server response" $ r ?== "response 1\n"

  terminateProcesses [serverP, clientP]


ncTestHunitClean = it "does a simple server <-> client interaction (2)" $ do

  -- Spawn
  (serverIn, serverOut, serverErr, serverP) <- run "nc" ["-l", "1234"]
  (clientIn, clientOut, clientErr, clientP) <- run "nc" ["localhost", "1234"]

  -- Check
  assertProcessesRunning serverP clientP

  -- Buffering
  setBuffering LineBuffering [clientIn, clientOut, serverIn, serverOut]

  -- Send
  assertLabel "server receives client request"  =<< popsOut clientIn serverOut "request 1\n"
  assertLabel "client receives server response" =<< popsOut serverIn clientOut "response 1\n"

  -- Close
  closeHandles [serverIn, serverOut, clientIn, clientOut]
  threadDelay (seconds 0.001)

  assertLabel "server shut down" =<< assertionExitSuccess serverP

  where
    ncWait h = asUtf8Str <$> waitOutput 10 100 h

    assertionRunning     proc = (?== Nothing)          <$> getProcessExitCode proc
    assertionExitSuccess proc = (?== Just ExitSuccess) <$> getProcessExitCode proc

    assertProcessesRunning serverP clientP = do
      assertLabel "server is running" =<< assertionRunning serverP
      assertLabel "client is running" =<< assertionRunning clientP

    popsOut hIn hOut content = do
      hPutStr hIn content
      r <- ncWait hOut
      return $ r ?== content


main = do
  -- catTest
  -- ncTest
  hspec $
    describe "netcat" $ do
      ncTestHunit
      -- it "wait a bit" $ do threadDelay 10000000
      ncTestHunitClean

-- TODO allow stdout/stderr redirection/fuse as in
--   - http://nominolo.blogspot.fr/2010/04/haskell-tip-redirect-stdout-in-haskell.html
--   - http://stackoverflow.com/questions/6736790/how-can-i-combine-handles-in-haskell
