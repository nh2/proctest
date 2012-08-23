{- | Helpers for asserting certain things for programs, using HUnit.

All of the assertions in this module throw HUnit exceptions on failure
using `assertFailure`.
-}

module Test.Proctest.Assertions (
  -- * Starting programs
    runAssert
  , assertExited

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
--
-- If the process is still running, a HUnit `assertFailure` exception is thrown.
assertExited :: ProcessHandle -> IO ()
assertExited p = do
  mE <- getProcessExitCode p
  when (mE == Nothing) $ assertFailure "The process is still running"
