-- | Parse RNAdistance output
--   For more information on RNAdistance consult: <>

module Bio.RNAdistanceParser (
                       parseRNAdistance,
                       readRNAdistance,
                       module Bio.RNAdistanceData
                      ) where

import Bio.RNAdistanceData
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad      

readInt :: String -> Int
readInt = read

-- | Parse the consenus of RNAz results         
genParserRNAdistance :: GenParser Char st RNAdistance
genParserRNAdistance = do
  string ("f: ")              
  distance <- many1 (noneOf " ")
  optional (many1 space)
  eof
  return $ RNAdistance (readInt distance)

-- | parse RNAdistance output from input string
parseRNAdistance input = parse genParserRNAdistance "genParseRNAdistance" input

-- | parse RNAdistance output from input filePath                      
readRNAdistance :: String -> IO (Either ParseError RNAdistance)                  
readRNAdistance filePath = parseFromFile genParserRNAdistance filePath

