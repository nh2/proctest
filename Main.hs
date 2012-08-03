{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

{-
Notes when using this module:
  - It needs to be compiled with "-threaded" for not blocking on process starts.
  - Beware that the Haskell GC closes process handles after their last use.
    If you don't want to be surprised by this, use hClose where you want
    them to be closed.
  - Be sure to use hSetBuffering appropriately, as processes run with BlockBuffering
    by default.
  - Dont run the program in a shell (e.g. runInteractiveCommand) if you want to
    be able to terminate it reliably (terminateProcess). Use processes without shells
    (runInteractiveProcess) instead.
-}

-- TODO allow setting hSetBinaryMode
-- TODO throw exception on timeout instead of returning maybe

module Main where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception hiding (assert)
import Data.Maybe (fromJust)
import Data.Text hiding (null)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable
import qualified Data.ByteString as BS
import System.Exit
import System.IO
import System.Process
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit



asUtf8 :: BS.ByteString -> Text
asUtf8 = decodeUtf8

asUtf8Str :: BS.ByteString -> String
asUtf8Str = unpack . asUtf8

run :: FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
run cmd args = runInteractiveProcess cmd args Nothing Nothing

terminateProcesses :: [ProcessHandle] -> IO ()
terminateProcesses = mapM_ terminateProcess

closeHandles :: [Handle] -> IO ()
closeHandles = mapM_ hClose

-- | Turns seconds into microseconds (throws error if they don't fit into Int).
seconds :: Float -> Int
seconds s = errorOnOverflow (round $ s * 10^6)
  where
    msg = "Overflow: " ++ show s ++ " seconds cannot be expressed as Int milliseconds"
    errorOnOverflow :: Integer -> Int
    errorOnOverflow i
      | i > fromIntegral (maxBound :: Int) = error msg
      | otherwise                          = fromIntegral i


_BUFSIZE = 2
_TIMEOUT_US = 3 * 10^6

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do
  mr <- timeout _TIMEOUT_US $ BS.hGetSome outh _BUFSIZE
  case mr of
    Nothing -> return () -- timeout
    Just r  -> if r == BS.empty
                 then putStrLn "empty" >> return ()
                 else print r >> mainloop inh outh

msToTimeoutUs :: Int -> Int
msToTimeoutUs ms
  | ms > ((maxBound :: Int) `div` 1000) = error "System.Timeout.timeout Int argument overflow"
  | otherwise                           = ms * 1000

data TimeoutException = TimeoutException deriving (Show, Typeable)

instance Exception TimeoutException

waitOutput :: Int -> Int -> Handle -> IO BS.ByteString
waitOutput timeout_ms maxBytes handle = let ex = throw TimeoutException in
  waitOutputNoEx timeout_ms maxBytes handle >>= maybe ex return

waitOutputNoEx :: Int -> Int -> Handle -> IO (Maybe BS.ByteString)
waitOutputNoEx timeout_ms maxBytes handle =
  timeout (msToTimeoutUs timeout_ms) (BS.hGetSome handle maxBytes)

ignoreTimeout = maybe BS.empty id


catTest = do
  (hIn, hOut, hErr, p) <- run "cat" []

  hSetBuffering hOut LineBuffering
  hSetBuffering hIn LineBuffering

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


setBuffering :: BufferMode -> [Handle] -> IO ()
setBuffering bufferMode handles = mapM_ (flip hSetBuffering bufferMode) handles


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
