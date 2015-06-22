-- | Parse RNAfold output
--   For more information on RNAfold consult: <http://www.tbi.univie.ac.at/RNA/RNAfold>

module Bio.RNAfoldParser (
                       systemRNAfold,
                       parseRNAfold,
                       readRNAfold,                                   
                       module Bio.RNAfoldData
                      ) where

import Bio.RNAfoldData
import Text.ParserCombinators.Parsec
import System.Process 
import System.Exit
import qualified Control.Exception.Base as CE

-- | Run external RNAfold command and read the output into the corresponding datatype
systemRNAfold :: String -> String -> IO ExitCode
systemRNAfold inputFilePath outputFilePath = system ("RNAfold --noPS  <" ++ inputFilePath  ++ " >" ++ outputFilePath)

readDouble :: String -> Double
readDouble = read              

-- | Parse the consenus of RNAz results         
genParserRNAfold :: GenParser Char st RNAfold
genParserRNAfold = do
  string (">") 
  _sequenceIdentifier <- many1 (noneOf "\n")                
  newline
  _sequence <- many1 (noneOf "\n")                
  newline 
  _secondaryStructure <- many1 (oneOf "&().,")
  space
  string ("(")
  _foldingEnergy <- many1 (noneOf ")")
  string (")")
  return $ RNAfold _sequenceIdentifier _sequence _secondaryStructure (readDouble _foldingEnergy)

-- | parse RNAfold output from input string
parseRNAfold :: [Char] -> Either ParseError RNAfold
parseRNAfold input = parse genParserRNAfold "genParseRNAfold" input

-- | parse RNAfold output from input filePath                      
readRNAfold :: String -> IO (Either ParseError RNAfold)                  
readRNAfold filePath = do
  parsedFile <- parseFromFile genParserRNAfold filePath
  CE.evaluate parsedFile
