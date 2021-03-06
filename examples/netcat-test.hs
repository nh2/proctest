module Main where


import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.Proctest
import Test.QuickCheck.Property (morallyDubiousIOProperty)


-- Example of communicating with `cat`.
--
-- See below for integration with hspec and hunit.
catTest :: IO ()
catTest = do
  (hIn, hOut, hErr, p) <- run "cat" []

  hPutStrLn hIn "test line 1"

  let catWait h = fmap asUtf8Str <$> waitOutputNoEx (seconds 0.01) 1000 h -- Wait max 10 ms, 1000 bytes

  sleep (seconds 0.00001) -- Give cat time to digest

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

{-
Some convenience for declaring tests:

  - '?@' for giving the label first, then the monadic action.

  - Separating assertions from their labels:

    -- '?==' creates an assertion

    -- 'label' optionally labels it
-}

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
  assert (LabeledAssertion msg (EqualAssertion actual expected)) =
    assertEqual msg actual expected

assertLabel str equalsAssertion = assert (label str equalsAssertion)




ncTestHunit = it "does a simple server <-> client interaction (1)" $ do

  (serverIn, serverOut, serverErr, serverP) <- run "nc" ["-l", "1234"]
  (clientIn, clientOut, clientErr, clientP) <- run "nc" ["localhost", "1234"]

  -- Make sure processes are running
  serverExitCode <- getProcessExitCode serverP
  clientExitCode <- getProcessExitCode clientP

  assertLabel "server is running" $ serverExitCode ?== Nothing
  assertLabel "client is running" $ clientExitCode ?== Nothing

  let ncWait h = asUtf8Str <$> waitOutput (seconds 0.01) 100 h

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
  setBuffering NoBuffering [clientIn, clientOut, serverIn, serverOut]

  -- Send
  assertLabel "server receives client request"  =<< popsOut clientIn serverOut "request 1\n"
  assertLabel "client receives server response" =<< popsOut serverIn clientOut "response 1\n"

  -- Close
  closeHandles [serverIn, serverOut, clientIn, clientOut]
  sleep (seconds 0.001)

  assertLabel "server shut down" =<< assertionExitSuccess serverP

  where
    ncWait h = asUtf8Str <$> waitOutput (seconds 0.01) 100 h

    assertionRunning     proc = (?== Nothing)          <$> getProcessExitCode proc
    assertionExitSuccess proc = (?== Just ExitSuccess) <$> getProcessExitCode proc

    assertProcessesRunning serverP clientP = do
      assertLabel "server is running" =<< assertionRunning serverP
      assertLabel "client is running" =<< assertionRunning clientP

    popsOut hIn hOut content = do
      hPutStr hIn content
      r <- ncWait hOut
      return $ r ?== content



catSpec = describe "cat" $ do
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


catCheck :: [String] -> IO Bool
catCheck lines = do

  (hIn, hOut, hErr, p) <- run "cat" []

  let catWait h = asUtf8Str <$> waitOutput (seconds 0.01) 1000 h

      checkLine l = do hPutStrLn hIn l
                       sleep (seconds 0.00001)
                       (== (l ++ "\n")) <$> catWait hOut

  and <$> (mapM checkLine lines)


catProp inputLines = morallyDubiousIOProperty $ catCheck inputLines

catPropSpec = describe "cat QuickCheck test" $ do
  prop "it gives back whatever we put in" catProp

main = do
  -- catTest -- This is not a hspec test.
  hspec $ do
    describe "cat" $ do
      catSpec
      catPropSpec
    describe "netcat" $ do
      ncTestHunit
      ncTestHunitClean
