{-# LANGUAGE DeriveDataTypeable #-}

{- | Read this first:

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

module Test.Proctest (
  -- * String helpers
  asUtf8
, asUtf8Str

 -- * Running and stopping programs
, run
, terminateProcesses
, closeHandles

-- * Time conversion
, seconds
, msToTimeoutUs

-- * Communicating with programs
, TimeoutException
, waitOutput
, waitOutputNoEx
, setBuffering

-- * Convenience module exports
, module System.Exit
, module System.IO
, module System.Process
) where

import Control.Exception (Exception (..), throwIO)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable
import qualified Data.ByteString as BS
import System.Exit
import System.IO
import System.Process
import System.Timeout (timeout)


asUtf8 :: BS.ByteString -> Text
asUtf8 = decodeUtf8

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


-- | Turns seconds into microseconds.
-- Throws error if they don't fit into Int.
--
-- Example: @(seconds 0.2)@ are 200 ms.
seconds :: Float -> Int
seconds s = errorOnOverflow (round $ s * 10000000)
  where
    msg = "Overflow: " ++ show s ++ " seconds cannot be expressed as Int milliseconds"
    errorOnOverflow :: Integer -> Int
    errorOnOverflow i
      | i > fromIntegral (maxBound :: Int) = error msg
      | otherwise                          = fromIntegral i


-- | Turns the given number of milliseconds into microseconds.
-- Throws error if they don't fit into Int.
msToTimeoutUs :: Int -> Int
msToTimeoutUs ms
  | ms > ((maxBound :: Int) `div` 1000) = error "System.Timeout.timeout Int argument overflow"
  | otherwise                           = ms * 1000


-- | Exception to be thrown when a program did not terminate
-- within the expected time.
data TimeoutException = TimeoutException deriving (Show, Typeable)

instance Exception TimeoutException


-- | Blocking wait for output on the given handle.
--
-- Returns 'Nothing' timeout is exceeded.
waitOutputNoEx :: Int                       -- ^ Timeout in milliseconds.
               -> Int                       -- ^ Maximum number of bytes to read.
               -> Handle                    -- ^ The handle to read from.
               -> IO (Maybe BS.ByteString)  -- ^ What was read from the handle.
waitOutputNoEx timeout_ms maxBytes handle =
  timeout (msToTimeoutUs timeout_ms) (BS.hGetSome handle maxBytes)

-- | Blocking wait for output on the given handle.
--
-- Throws a 'TimeoutException' if the timeout is exceeded.
--
-- Based on 'waitOutputNoEx'.
waitOutput :: Int               -- ^ Timeout in milliseconds.
           -> Int               -- ^ Maximum number of bytes to read.
           -> Handle            -- ^ The handle to read from.
           -> IO BS.ByteString  -- ^ What was read from the handle.
waitOutput timeout_ms maxBytes handle = let ex = throwIO TimeoutException in
  waitOutputNoEx timeout_ms maxBytes handle >>= maybe ex return


-- | Sets the buffering of the all given handles to the given 'BufferMode'.
setBuffering :: BufferMode -> [Handle] -> IO ()
setBuffering bufferMode handles = mapM_ (flip hSetBuffering bufferMode) handles
