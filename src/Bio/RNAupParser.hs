-- | Parse RNAup output
--   For more information on RNAup consult: <http://www.tbi.univie.ac.at/RNA/RNAup.html>

module Bio.RNAupParser (
                       parseRNAup,
                       readRNAup,                                   
                       module Bio.RNAupData
                      ) where

import Bio.RNAupData
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse the input as list of RNAupInteraction datatype
parseRNAupOutput :: GenParser Char st [RNAplexInteraction]
parseRNAupOutput = do
  rnaUpInteractions <- many1 (try parseRNAplexInteraction)   
  eof  
  return $ rnaUpInteractions

-- | Parse the consenus of RNAup results         
parseRNAupInteraction :: GenParser Char st RNAupInteraction
parseRNAupInteraction = do
  string (">") 
  targetIdentifier <- many1 (noneOf "\n")                
  newline
  string (">") 
  queryIdentifier <- many1 (noneOf "\n")                
  newline 
  secondaryStructure <- many1 (oneOf "&().,")
  many1 space
  targetDuplexBegin <- many1 digit
  char ','
  targetDuplexEnd <- many1 digit
  many1 space
  char ':'
  many1 space
  queryDuplexBegin <- many1 digit
  char ','
  queryDuplexEnd <- many1 digit
  many1 space
  char '('
  duplexEnergy <- many1 (noneOf (" )"))
  optional space
  optional (char '=')
  optional space
  duplexEnergyWithoutAccessiblity <- optionMaybe (try (many1 (noneOf (" )"))))
  optional space 
  optional (char '+')
  optional (many1 space)
  queryAccessiblity <- optionMaybe (try (many1 (noneOf (" )")))) 
  optional space 
  optional (char '+')
  optional (many1 space)
  targetAccessibility <- optionMaybe (try (many1 (noneOf (")"))))
  char ')'
  newline
  return $ RNAupInteraction targetIdentifier queryIdentifier secondaryStructure (readInt targetDuplexBegin) (readInt targetDuplexEnd) (readInt queryDuplexBegin) (readInt queryDuplexEnd) (readDouble duplexEnergy) (liftM readDouble duplexEnergyWithoutAccessiblity) (liftM readDouble queryAccessiblity) (liftM readDouble targetAccessibility)

-- | parse RNAupOutput from input string
parseRNAup input = parse parseRNAupOutput "parseRNAupOutput" input

-- | parse from input filePath                      
readRNAup :: String -> IO (Either ParseError [RNAupInteraction])                  
readRNAup filePath = parseFromFile parseRNAupOutput filePath
