-- | Parse RNAplex output
--   For more information on RNAplex consult: <http://www.bioinf.uni-leipzig.de/Software/RNAplex/>

module Bio.RNAplexParser (
                       parseRNAplex,
                       readRNAplex,                                   
                       module Bio.RNAplexData
                      ) where

import Bio.RNAplexData
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad
--import Control.Applicative

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse the input as RNAzOutput datatype
parseRNAplexOutput :: GenParser Char st RNAplexOutput
parseRNAplexOutput = do
  rnaPlexInteractions <- many1 (try parseRNAplexInteraction)   
  eof  
  return $ RNAplexOutput rnaPlexInteractions

-- | Parse the consenus of RNAz results         
parseRNAplexInteraction :: GenParser Char st RNAplexInteraction
parseRNAplexInteraction = do
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
  space
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
  duplexEnergyWithoutAccessiblity <- optionMaybe (many1 (noneOf (" ")))
  optional space 
  optional (char '+')
  optional (many1 space)
  queryAccessiblity <- optionMaybe (many1 (noneOf (" "))) 
  optional space 
  optional (char '+')
  optional (many1 space)
  targetAccessibility <- optionMaybe (many1 (noneOf (")"))) 
  char ')'
  newline 
  return $ RNAplexInteraction targetIdentifier queryIdentifier secondaryStructure (readInt targetDuplexBegin) (readInt targetDuplexEnd) (readInt queryDuplexBegin) (readInt queryDuplexEnd) (readDouble duplexEnergy) (liftM readDouble duplexEnergyWithoutAccessiblity) (liftM readDouble queryAccessiblity) (liftM readDouble targetAccessibility)

-- | parse RNAplexOutput from input string
parseRNAplex input = parse parseRNAplexOutput "parseRNAplexOutput" input

-- | parse from input filePath                      
readRNAplex :: String -> IO (Either ParseError RNAplexOutput)                  
readRNAplex filePath = parseFromFile parseRNAplexOutput filePath
