{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE GADTs #-}

{-
  Modified version of the original code based on changes from "oren 2013-02-19"

  http://book.realworldhaskell.org/read/systems-programming-in-haskell.html
  http://ideone.com/tj4QRj

  - Removed unnecessary MVar
  - Fixed exception handling
  - Using GADTs
-}

module RunProcessSimple where

import Control.Concurrent (forkIO)
import Control.Exception (catch, SomeException(..))
import System.IO (hClose, hGetContents, hPutStr)
import System.Exit (ExitCode(..))
import System.Posix.Process (executeFile, forkProcess, getProcessStatus, ProcessStatus(..))
import System.Posix.IO (closeFd, fdToHandle, createPipe, dupTo, stdInput, stdOutput)
import System.Posix.Types (Fd)

{- | The type for running external commands.  The first part
of the tuple is the program name.  The list represents the
command-line parameters to pass to the command. -}
type SysCommand = (String, [String])

{- | The result of running any command -}
data CommandResult = CommandResult {
    cmdOutput :: IO String,              -- ^ IO action that yields the output
    getExitStatus :: IO ProcessStatus,   -- ^ IO action that yields exit result
    fds :: [Fd]                          -- ^ List of FDs to be closed
    }

{- | The type for handling global lists of FDs to always close in the clients
-}
type CloseFDs = [Fd]

{- | Class representing anything that is a runnable command -}
class CommandLike a where
    {- | Given the command and a String representing input,
         invokes the command.  Returns a String
         representing the output of the command. -}
    invoke :: a -> CloseFDs -> String -> IO CommandResult

-- Support for running system commands
instance CommandLike SysCommand where
    invoke (cmd, args) closefds input =
        do -- Create two pipes: one to handle stdin and the other
           -- to handle stdout.  We do not redirect stderr in this program.
           (stdinread, stdinwrite) <- createPipe
           (stdoutread, stdoutwrite) <- createPipe

           -- We add the parent FDs to this list because we always need
           -- to close them in the clients.
           let closefds2 = closefds ++ [stdinwrite, stdoutread]

           -- Now, grab the closed FDs list and fork the child.
           childPID <- forkProcess (child closefds2 stdinread stdoutwrite)

           -- Now, on the parent, close the client-side FDs.
           closeFd stdinread
           closeFd stdoutwrite

           -- Write the input to the command.
           stdinhdl <- fdToHandle stdinwrite
           forkIO $ do hPutStr stdinhdl input
                       hClose stdinhdl

           -- Prepare to receive output from the command
           stdouthdl <- fdToHandle stdoutread

           -- Set up the function to call when ready to wait for the
           -- child to exit.
           let waitfunc =
                do status <- getProcessStatus True False childPID
                   case status of
                       Nothing -> fail $ "Error: Nothing from getProcessStatus"
                       Just ps -> return ps
           return $ CommandResult { cmdOutput = hGetContents stdouthdl,
                                    getExitStatus = waitfunc,
                                    fds = closefds2 }

        -- Define what happens in the child process
        where child closefds stdinread stdoutwrite =
                do -- Copy our pipes over the regular stdin/stdout FDs
                   dupTo stdinread stdInput
                   dupTo stdoutwrite stdOutput

                   -- Now close the original pipe FDs
                   closeFd stdinread
                   closeFd stdoutwrite

                   -- Close all the open FDs we inherited from the parent
                   mapM_ (\fd -> catch (closeFd fd) (\(SomeException _) -> return ())) closefds

                   -- Start the program
                   executeFile cmd True args Nothing


{- | Type representing a pipe.  A 'PipeCommand' consists of a source
and destination part, both of which must be instances of
'CommandLike'. -}
data PipeCommand src dst where
    PipeCommand :: (CommandLike src, CommandLike dest) => src -> dest -> PipeCommand src dest

{- | A convenient function for creating a 'PipeCommand'. -}
(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

{- | Make 'PipeCommand' runnable as a command -}
instance (CommandLike a, CommandLike b) =>
         CommandLike (PipeCommand a b) where
    invoke (PipeCommand src dest) closefds input =
        do res1 <- invoke src closefds input
           output1 <- cmdOutput res1
           let fds1 = fds res1
           res2 <- invoke dest fds1 output1
           let fds2 = fds res2
           return $ CommandResult (cmdOutput res2) (getEC res1 res2) fds2

{- | Given two 'CommandResult' items, evaluate the exit codes for
both and then return a "combined" exit code.  This will be ExitSuccess
if both exited successfully.  Otherwise, it will reflect the first
error encountered. -}
getEC :: CommandResult -> CommandResult -> IO ProcessStatus
getEC src dest =
    do sec <- getExitStatus src
       dec <- getExitStatus dest
       case sec of
            Exited ExitSuccess -> return dec
            x -> return x

{- | Execute a 'CommandLike'. -}
runIO :: CommandLike a => a -> IO ()
runIO cmd =
    do -- Initialize our closefds list
       let closefds = []

       -- Invoke the command
       res <- invoke cmd closefds []

       -- Process its output
       output <- cmdOutput res
       putStr output

       -- Wait for termination and get exit status
       ec <- getExitStatus res
       case ec of
            Exited ExitSuccess -> return ()
            x -> fail $ "Exited: " ++ show x