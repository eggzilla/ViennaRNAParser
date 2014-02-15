-- | Parse RNAalifold output
--   For more information on RNAalifold consult: <>

module Bio.RNAalifoldParser (
                       parseRNAalifold,
                       readRNAalifold,                                 
                       module Bio.RNAalifoldData
                      ) where

import Bio.RNAalifoldData
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse the consenus of RNAz results         
genParserRNAalifold :: GenParser Char st RNAalifoldOutput
genParserRNAalifold = do
  sequence <- many1 (noneOf "\n")                
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
  return $ RNAalifoldOutput sequence secondaryStructure (readDouble foldingEnergy) (readDouble initialFoldingEnergy) (readDouble covarianceContributionEnergy)   
-- | parse RNAalifold output from input string
parseRNAalifold input = parse genParserRNAalifold "genParseRNAalifold" input

-- | parse RNAalifold output from input filePath                      
readRNAalifold :: String -> IO (Either ParseError RNAalifoldOutput)                  
readRNAalifold filePath = parseFromFile genParserRNAalifold filePath

