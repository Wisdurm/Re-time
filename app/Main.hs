module Main where

import Paths_retime (version)
import Data.Version (showVersion)
import System.Environment
import System.Console.Readline
import System.Console.ANSI
import System.IO (stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.IORef
import Retime.Libraries
import Retime.Interpreter.Types
import Retime

logo :: String
logo = "\
\   ___        _   _           \n\
\  / _ \\___ __| |_(_)_ __  ___ \n\
\ / , _/ -_)__|  _| | '  \\/ -_)\n\
\/_/|_|\\__/    \\__|_|_|_|_\\___|"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putInfo
      symRef <- defaultSymtab
      repl symRef
    (file:_) -> do
      -- TODO: cargs
      contents <- T.readFile file
      out <- interpretText contents
      putStrLn out

putInfo :: IO ()
putInfo = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then do
    setSGR [SetColor Foreground Dull Yellow]
    putStrLn logo
    setSGR [Reset]
    putStrLn ("Retime " ++ showVersion version)
    else do
    putStrLn ("Retime " ++ showVersion version)

repl :: IORef Symtab -> IO ()
repl symRef = do
  maybeLine <- readline ">> "
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      addHistory line
      out <- interpretTextWithSym symRef (T.pack line)
      putStrLn out
      repl symRef
