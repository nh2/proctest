{- | Helpers for asserting certain things for programs, using HUnit.

All of the assertions in this module throw HUnit exceptions on failure
using `assertFailure`.
-}

module Test.Proctest.Assertions (
  -- * Starting programs
    runAssert
  , assertExited
  , _PROCTEST_POLL_TIMEOUT
  , assertExitedTimeout

) where


import Control.Monad
import Test.Proctest
import Test.HUnit


-- | Performs the monadic action on the contents of the `Just`, if any.
onJust :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
onJust = maybe (return ())


-- | Runs the given program with `run` and asserts that it is still running
-- after the given timeout.
--
-- Don't choose the timeout too high as this function will block for it.
--
-- If the timeout is exceeded, a HUnit `assertFailure` exception is thrown,
-- showing the command line to be invoked, the exit code, and the standard
-- error output of the program.
runAssert :: Timeout -> FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
runAssert timeout cmd args = do
  r@(_, _, hErr, p) <- run cmd args
  sleep timeout
  getProcessExitCode p >>= (onJust $ \ec -> do
    -- The lazy IO here is OK, as we immediately show the String afterwards.
    err <- hGetContents hErr
    assertFailure $ "The program '" ++ program ++ "' after being started immediately "
                    ++ "exited with exit code " ++ show ec ++ ".\n"
                    ++ "--- stderr: ---\n" ++ err ++ "\n--- End of stderr ---")
  return r
  where
    program = cmd ++ concatMap (" " ++) args


-- | Asserts that the given process has shut down.
--
-- You might need to `sleep` before to give the process time to exit.
-- It is usually better to use `assertExitedTimeout` in those cases.
--
-- If the process is still running, a HUnit `assertFailure` exception is thrown.
assertExited :: ProcessHandle -> IO ()
assertExited p = do
  mE <- getProcessExitCode p
  when (mE == Nothing) $ assertFailure "The process is still running"


-- | How often to poll in waiting functions with maximum timeout.
_PROCTEST_POLL_TIMEOUT :: Timeout
_PROCTEST_POLL_TIMEOUT = mkTimeoutMs (1 :: Int)


-- | HUnit's `assertFailure` currently does not allow returning any type
-- like normal throw functions. This is a workaround.
--
-- Usage:
--
-- >fixHunitFailure $ assertFailure "boo!"
--
-- TODO Remove this in case it gets fixed in HUnit.
fixHunitFailure :: IO () -> IO a
fixHunitFailure = fmap . const $ error "Test.Proctest.Assertions: Executing after assertFailure, cannot happen!"


-- | Asserts that the given process has shut down in *at most* the given timeout.
--
-- Periodically polling with `_PROCTEST_POLL_TIMEOUT`,
-- returns as soon as the application has terminated or the timeout is exceeded.
--
-- Use this to write faster tests than with manual `sleep`ing:
-- For most tests, the application will actually finish way before the timeout.
--
-- If the process is still running, a HUnit `assertFailure` exception is thrown.
assertExitedTimeout :: Timeout -> ProcessHandle -> IO ExitCode
assertExitedTimeout timeout p =
  -- withTimeout timeout loop >>= maybe failure return
  -- TODO Fix HUnit.
  withTimeout timeout loop >>= maybe (fixHunitFailure failure) return
  where
    failure = assertFailure "The process is still running"
    loop = getProcessExitCode p >>= maybe loop return
