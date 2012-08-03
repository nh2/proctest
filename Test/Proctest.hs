{-# LANGUAGE DeriveDataTypeable #-}

{- | An IO library for testing interactive command line programs.

Read this first:

  - Tests using Proctests need to be compiled with @-threaded@ for not blocking on process spawns.

  - Beware that the Haskell GC closes process 'Handle's after their last use.
    If you don't want to be surprised by this, use 'hClose' where you want
    them to be closed (convenience: 'closeHandles').

  - Be sure to use 'hSetBuffering' appropriately, as processes run with 'BlockBuffering'
    by default (convenience: 'setBuffering').

  - Dont run the program in a shell (e.g. 'runInteractiveCommand') if you want to
    be able to terminate it reliably ('terminateProcess'). Use processes without shells
    ('runInteractiveProcess') instead.


Example:

Let's say you want to test an interactive command line program like @cat@,
and integrate your test into a test framework like "Test.HSpec",
using "Test.HSpec.HUnit" for the IO parts (remember that Proctest /is/ stateful IO).

> main = hspec $ describe "cat" $ do
>
>   it "prints out what we put in" $ do
>
>     -- Start up the program to test
>     (hIn, hOut, hErr, p) <- run "cat" []
>
>     -- Make sure buffering doesn't prevent us from reading what we expect
>     setBuffering LineBuffering [hIn, hOut]
>
>     -- Communicate with the program
>     hPutStrLn hIn "hello world"
>
>     -- Define a convenient wrapper around 'waitOutput'.
>     --
>     -- It specifies how long we have to wait
>     -- (malfunctioning programs shall not block automated testing for too long)
>     -- and how many bytes we are sure the expected response fits into
>     -- (malfunctioning programs shall not flood us with garbage either).
>     let catWait h = asUtf8Str <$> waitOutput (seconds 0.01) 1000 h -- Wait max 10 ms, 1000 bytes
>
>     -- Read the response
>     response <- catWait hOut
>
>     -- Test if it is what we want (here using HUnit's 'expectEqual')
>     response @?= "hello world\n"

-}

module Test.Proctest (
  -- * String conversion
  asUtf8
, asUtf8Str

 -- * Running and stopping programs
, run
, terminateProcesses
, closeHandles

-- * Timeouts
, Timeout (NoTimeout)
, InvalidTimeoutError
, mkTimeoutUs
, mkTimeoutMs
, mkTimeoutS
, seconds

-- * Communicating with programs
, TimeoutException
, waitOutput
, waitOutputNoEx
, setBuffering
, sleep

-- * Convenience module exports
, module System.Exit
, module System.IO
, module System.Process
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..), throw, throwIO)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable
import qualified Data.ByteString as BS
import System.Exit
import System.IO
import System.Process
import System.Timeout (timeout)


-- | Treats a 'BS.ByteString' as UTF-8 decoded 'Text'.
asUtf8 :: BS.ByteString -> Text
asUtf8 = decodeUtf8

-- | Treats a 'BS.ByteString' as UTF-8 decoded 'String'.
asUtf8Str :: BS.ByteString -> String
asUtf8Str = unpack . asUtf8


-- | Runs a program with the given arguemtns.
--
-- Returns @(stdout, stderr, stdin, process)@. See 'runInteractiveProcess'.
--
-- Directly runs the process, does not use a shell.
run :: FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
run cmd args = runInteractiveProcess cmd args Nothing Nothing

-- | Terminates all processes in the list.
terminateProcesses :: [ProcessHandle] -> IO ()
terminateProcesses = mapM_ terminateProcess

-- | Closes all handles in the list.
closeHandles :: [Handle] -> IO ()
closeHandles = mapM_ hClose


-- | A microsecnd timeout, or 'NoTimeout'.
data Timeout = NoTimeout | Micros Int

-- | An error to be thrown if something is to be converted into timeout
-- that does not fit into 'Int'.
data InvalidTimeoutError = InvalidTimeoutError String deriving (Show, Typeable)

instance Exception InvalidTimeoutError

-- | Turns the given number of microsecnds into a 'Timeout'.
--
-- Throws an exception on 'Int' overflow.
mkTimeoutUs :: Integer -> Timeout
mkTimeoutUs n
  | n <= 0                             = NoTimeout
  | n > fromIntegral (maxBound :: Int) = throw $ InvalidTimeoutError msg
  | otherwise                          = Micros (fromIntegral n)
  where
    msg = "Test.Proctest.Timeout: " ++ show n ++ " microseconds do not fit into Int"


-- | Turns the given number of milliseconds into a 'Timeout'.
--
-- Throws an exception on 'Int' overflow.
mkTimeoutMs :: (Integral a) => a -> Timeout
mkTimeoutMs = mkTimeoutUs . (* 1000) . fromIntegral


-- | Turns the given number of seconds into a 'Timeout'.
--
-- Throws an exception on 'Int' overflow.
mkTimeoutS :: (Integral a) => a -> Timeout
mkTimeoutS = mkTimeoutUs . (* 1000000) . fromIntegral


-- | Turns floating seconds into a 'Timeout'.
--
-- Throws an exception on 'Int' overflow.
--
-- Example: @(seconds 0.2)@ are roughly @Micros 200000@.
seconds :: Float -> Timeout
seconds s = mkTimeoutUs (round $ s * 10000000)


-- | Suspends execution for the given timeout; uses 'threadDelay' internally.
-- For 'NoTimeout', threadDelay will not be called.
sleep :: Timeout -> IO ()
sleep t = case t of
  NoTimeout -> return ()
  Micros n  -> threadDelay n


-- | Exception to be thrown when a program did not terminate
-- within the expected time.
data TimeoutException = TimeoutException deriving (Show, Typeable)

instance Exception TimeoutException


-- | Converts a 'Timeout' milliseconds suitable to be passed into 'timeout'.
timeoutToSystemTimeoutArg :: Timeout -> Int
timeoutToSystemTimeoutArg t = case t of
  Micros n  -> n
  NoTimeout -> -1

-- | Blocking wait for output on the given handle.
--
-- Returns 'Nothing' timeout is exceeded.
waitOutputNoEx :: Timeout                   -- ^ Timeout after which reading output will be aborted.
               -> Int                       -- ^ Maximum number of bytes after which reading output will be aborted.
               -> Handle                    -- ^ The handle to read from.
               -> IO (Maybe BS.ByteString)  -- ^ What was read from the handle.
waitOutputNoEx t maxBytes handle =
  timeout (timeoutToSystemTimeoutArg t) (BS.hGetSome handle maxBytes)


-- | Blocking wait for output on the given handle.
--
-- Throws a 'TimeoutException' if the timeout is exceeded.
--
-- Based on 'waitOutputNoEx'.
waitOutput :: Timeout           -- ^ Timeout after which reading output will be aborted.
           -> Int               -- ^ Maximum number of bytes after which reading output will be aborted.
           -> Handle            -- ^ The handle to read from.
           -> IO BS.ByteString  -- ^ What was read from the handle.
waitOutput t maxBytes handle = let ex = throwIO TimeoutException in
  waitOutputNoEx t maxBytes handle >>= maybe ex return


-- | Sets the buffering of the all given handles to the given 'BufferMode'.
setBuffering :: BufferMode -> [Handle] -> IO ()
setBuffering bufferMode handles = mapM_ (flip hSetBuffering bufferMode) handles
