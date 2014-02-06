-- | Parse RNAfold output
--   For more information on RNAplex consult: <>

module Bio.RNAfoldParser (
                       parseRNAfold,
                       readRNAfold,                                   
                       module Bio.RNAfoldData
                      ) where

import Bio.RNAfoldData
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse the input as RNAfold datatype
genParserRNAfolds :: GenParser Char st RNAfoldOutput
genParserRNAfolds = do
  rnafolds <- many1 genParserRNAfold
  eof  
  return $ RNAfoldOutput rnafolds

-- | Parse the consenus of RNAz results         
genParserRNAfold :: GenParser Char st RNAfold
genParserRNAfold = do
  string (">") 
  sequenceIdentifier <- many1 (noneOf "\n")                
  newline
  sequence <- many1 (noneOf "\n")                
  newline 
  secondaryStructure <- many1 (oneOf "&().,")
  space
  string ("(")
  foldingEnergy <- many1 (noneOf "\n")
  string (")")
  return $ RNAfold sequenceIdentifier sequence secondaryStructure (readDouble foldingEnergy)

-- | parse RNAfold output from input string
parseRNAfold input = parse genParserRNAfold "genParseRNAfold" input

-- | parse RNAfold output from input filePath                      
readRNAfold :: String -> IO (Either ParseError RNAfold)                  
readRNAfold filePath = parseFromFile genParserRNAfold filePath
