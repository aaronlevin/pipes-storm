{-# LANGUAGE OverloadedStrings #-}

module Pipes.Storm.System
(
    writePid
        ) where

import System.Posix.Process (getProcessID)

-- Write the PID of the process to a file
writePid :: IO ()
writePid = do
    pid <- getProcessID
    writeFile (show pid) ""
