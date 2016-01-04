-- | Parser test script
-- runghc -package-db --ghc-arg=.cabal-sandbox/x86_64-linux-ghc-7.10.3-packages.conf.d/  ViennaRNAParserTest.hs RNAup_interaction_first.out
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
import System.Process
import Control.Monad    
import Data.Either
import Data.Either.Unwrap
    
main = do
  args <- getArgs
  let input_file = (head args)
  putStrLn "Test:"
  parsedinput <-  readRNAcode input_file  
  print parsedinput

