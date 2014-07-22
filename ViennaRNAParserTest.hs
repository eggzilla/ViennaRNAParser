-- | Parser test script
--   read from file and directly print parsing output

module Main where
    
import System.Environment (getArgs)
import System.Process 
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import Data.List
import Bio.ViennaRNAParser
import System.Directory
import System.Cmd
import Control.Monad    
import Data.Either
import Data.Either.Unwrap
    
main = do
  args <- getArgs
  let input_file = (head args)
  putStrLn "Test:"
  parsedinput <- readRNAplex input_file  
  print parsedinput

