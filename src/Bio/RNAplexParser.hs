-- | Parse RNAplex output
--   For more information on RNAplex consult: <http://www.bioinf.uni-leipzig.de/Software/RNAplex/>

module Bio.RNAplexParser (
                       parseRNAplex,
                       readRNAplex,                                   
                       module Bio.RNAplexData
                      ) where

import Bio.RNAplexData
import Bio.ViennaRNAParserLibrary
import Text.ParserCombinators.Parsec    
import Control.Monad

-- | Parse the input as list of RNAplexInteraction datatype
parseRNAplexOutput :: GenParser Char st [RNAplexInteraction]
parseRNAplexOutput = do
  rnaPlexInteractions <- many1 (try parseRNAplexInteraction)   
  eof  
  return $ rnaPlexInteractions

-- | Parse the consenus of RNAplex results         
parseRNAplexInteraction :: GenParser Char st RNAplexInteraction
parseRNAplexInteraction = do
  string (">") 
  _targetIdentifier <- many1 (noneOf "\n")                
  newline
  string (">") 
  _queryIdentifier <- many1 (noneOf "\n")                
  newline 
  _secondaryStructure <- many1 (oneOf "&().,")
  many1 space
  _targetDuplexBegin <- many1 digit
  char ','
  _targetDuplexEnd <- many1 digit
  many1 space
  char ':'
  many1 space
  _queryDuplexBegin <- many1 digit
  char ','
  _queryDuplexEnd <- many1 digit
  many1 space
  char '('
  optional space
  _duplexEnergy <- many1 (noneOf (" )"))
  optional space
  optional (char '=')
  optional space
  _duplexEnergyWithoutAccessiblity <- optionMaybe (try (many1 (noneOf (" )"))))
  optional space 
  optional (char '+')
  optional (many1 space)
  _queryAccessiblity <- optionMaybe (try (many1 (noneOf (" )")))) 
  optional space 
  optional (char '+')
  optional (many1 space)
  _targetAccessibility <- optionMaybe (try (many1 (noneOf (")"))))
  char ')'
  newline
  return $ RNAplexInteraction _targetIdentifier _queryIdentifier _secondaryStructure (readInt _targetDuplexBegin) (readInt _targetDuplexEnd) (readInt _queryDuplexBegin) (readInt _queryDuplexEnd) (readDouble _duplexEnergy) (liftM readDouble _duplexEnergyWithoutAccessiblity) (liftM readDouble _queryAccessiblity) (liftM readDouble _targetAccessibility)

-- | parse RNAplexOutput from input string
parseRNAplex :: [Char] -> Either ParseError [RNAplexInteraction]
parseRNAplex input = parse parseRNAplexOutput "parseRNAplexOutput" input

-- | parse from input filePath                      
readRNAplex :: String -> IO (Either ParseError [RNAplexInteraction])                  
readRNAplex filePath = parseFromFile parseRNAplexOutput filePath
