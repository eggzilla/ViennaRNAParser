-- | Parse RNAalifold output
--   For more information on RNAalifold consult: <>

module Bio.RNAalifoldParser (
                       systemRNAalifold,
                       parseRNAalifold,
                       readRNAalifold,                                 
                       module Bio.RNAalifoldData
                      ) where

import Bio.RNAalifoldData
import Text.ParserCombinators.Parsec
import System.Process
import System.Exit
import qualified Control.Exception.Base as CE

readDouble :: String -> Double
readDouble = read              

--- | Run external RNAalifold command and read the output into the corresponding datatype
systemRNAalifold :: String -> String -> String -> IO ExitCode
systemRNAalifold options inputFilePath outputFilePath = system ("RNAalifold " ++ options  ++ " < " ++ inputFilePath  ++ " > " ++ outputFilePath)

-- | Parse the consenus of RNAz results         
genParserRNAalifold :: GenParser Char st RNAalifoldOutput
genParserRNAalifold = do
  _sequence <- many1 (noneOf "\n")                
  newline
  secondaryStructure <- many1 (oneOf "&().,")
  string (" (")
  foldingEnergy <- many1 (noneOf " ")
  string " ="
  many1 space
  initialFoldingEnergy <- many1 (noneOf " ")
  string (" +")
  many1 space
  covarianceContributionEnergy <- many1 (noneOf ")")
  string (")")
  many1 space
  eof
  return $ RNAalifoldOutput _sequence secondaryStructure (readDouble foldingEnergy) (readDouble initialFoldingEnergy) (readDouble covarianceContributionEnergy)
   
-- | parse RNAalifold output from input string
parseRNAalifold :: [Char] -> Either ParseError RNAalifoldOutput
parseRNAalifold input = parse genParserRNAalifold "genParseRNAalifold" input

-- | parse RNAalifold output from input filePath                      
readRNAalifold :: String -> IO (Either ParseError RNAalifoldOutput)                  
readRNAalifold filePath = do
  parsedFile <- parseFromFile genParserRNAalifold filePath
  CE.evaluate parsedFile

