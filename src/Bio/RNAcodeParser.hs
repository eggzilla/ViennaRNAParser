-- | Parse RNAcode output
--   For more information on RNAcode consult: <http://wash.github.io/rnacode/>
module Bio.RNAcodeParser (
                       systemRNAcode,
                       parseRNAcode,
                       readRNAcode,                                   
                       module Bio.RNAcodeData
                      ) where

import Bio.RNAcodeData
import Bio.ViennaRNAParserLibrary
import Text.ParserCombinators.Parsec
import System.Process
import System.Exit
import Text.Parsec.Token
import qualified Control.Exception.Base as CE
import Text.Parsec.Language (haskellDef)

hTokenParser = makeTokenParser haskellDef

-- | Run external RNAcode command and read the output into the corresponding datatype
systemRNAcode :: String -> String -> String -> IO ExitCode
systemRNAcode options inputFilePath outputFilePath = system ("RNAcode " ++ options ++ " " ++ inputFilePath ++ " >" ++ outputFilePath)

-- | Parse the input as RNAcode datatype
genParseRNAcode :: GenParser Char st RNAcode
genParseRNAcode = do
  many1 (oneOf " \n")
  string "HSS # Frame Length  From    To        Name       Start         End    Score        P"
  newline
  string "======================================================================================"
  _rnacodeHits <- many1 genParseRNAcodeHit
  newline
  _alignmentnumber <- natural hTokenParser
  string " alignment(s) scored in "
  _time <- float hTokenParser
  string " seconds. Parameters used:"
  newline
  string "N="
  _samples <- natural hTokenParser
  string ", Delta="
  _delta <- float hTokenParser
  string ", Omega="
  _bigomega <- float hTokenParser
  string ", omega="
  _smallomega <- float hTokenParser
  string ", stop penalty="
  _stopPenalty <- float hTokenParser
  return $ RNAcode _rnacodeHits (Just (fromInteger _alignmentnumber)) (Just _time) (Just (fromInteger _samples)) (Just _delta) (Just _bigomega) (Just _smallomega) (Just _stopPenalty)

-- | Parse the input as RNAcodeHit
genParseRNAcodeHit :: GenParser Char st RNAcodeHit
genParseRNAcodeHit = do
  many1 (char ' ')
  _hss <- natural hTokenParser
  _frame <- integer hTokenParser
  _length <- natural hTokenParser
  _from <- natural hTokenParser
  _to <- natural hTokenParser
  _name <- many1 (noneOf " ")
  many1 (char ' ')
  _start <- natural hTokenParser
  _end <- natural hTokenParser
  _score <- float hTokenParser
  _pvalue <- float hTokenParser
  newline
  return $ RNAcodeHit (fromInteger _hss) (fromInteger _frame) (fromInteger _length) (fromInteger _from) (fromInteger _to) _name (fromInteger _start) (fromInteger _end) _score _pvalue

    
-- | parse RNAcode from input string
parseRNAcode :: [Char] -> Either ParseError RNAcode
parseRNAcode input = parse genParseRNAcode "parseRNAcode" input

-- | parse RNAcode from input filePath                      
readRNAcode :: String -> IO (Either ParseError RNAcode)                  
readRNAcode filePath = do 
  parsedFile <- parseFromFile genParseRNAcode filePath
  CE.evaluate parsedFile
