-- | Parse RNAcode output
--   For more information on RNAcode consult: <http://wash.github.io/rnacode/>


module Bio.RNAcodeParser (
                       systemRNAcode,
                       parseRNAcode,
                       readRNAcode,
                       parseRNAcodeTabular,
                       readRNAcodeTabular,
                       module Bio.RNAcodeData
                      ) where
import Data.Functor.Identity
import Bio.RNAcodeData
import Text.ParserCombinators.Parsec
import System.Process
import System.Exit
import Text.Parsec.Token
import qualified Control.Exception.Base as CE
import Text.Parsec.Language (haskell)
import Control.Applicative ((<*>),(<$>),(<$),pure)

-- | Run external RNAcode command and read the output into the corresponding datatype
systemRNAcode :: String -> String -> String -> IO ExitCode
systemRNAcode options inputFilePath outputFilePath = system ("RNAcode " ++ options ++ " " ++ inputFilePath ++ " >" ++ outputFilePath)

-- | Parse the tabular input as RNAcode datatype
genParseRNAcodeTabular :: GenParser Char st RNAcode
genParseRNAcodeTabular = do
  _rnacodeHits <- many1 genParseRNAcodeTabularHit
  return $ RNAcode _rnacodeHits Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Parse the input as RNAcodeHit
genParseRNAcodeTabularHit :: GenParser Char st RNAcodeHit
genParseRNAcodeTabularHit = do
  _hss <- natural haskell
  _frame <- integer haskell
  _length <- natural haskell
  _from <- natural haskell
  _to <- natural haskell
  _name <- identifier haskell
  --_name <- identifier haskell
  _name <- many1 (oneOf "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz1234567890_-")
  string ("\t")
  _start <- natural haskell
  _end <- natural haskell
  _score <- float haskell
  _pvalue <- float haskell
  return $ RNAcodeHit (fromInteger _hss) (fromInteger _frame) (fromInteger _length) (fromInteger _from) (fromInteger _to) _name (fromInteger _start) (fromInteger _end) _score _pvalue 


-- | Parse the input as RNAcode datatype
genParseRNAcode :: GenParser Char st RNAcode
genParseRNAcode = do
  many1 (oneOf " \n")
  string "HSS # Frame Length  From    To        Name       Start         End    Score        P"
  newline
  string "======================================================================================"
  newline
  _rnacodeHits <- many1 (try genParseRNAcodeHit)
  newline
  _alignmentnumber <- natural haskell
  string "alignment(s) scored in "
  _time <- float haskell
  string "seconds. Parameters used:"
  newline
  string "N="
  _samples <- natural haskell
  string ", Delta="
  --_delta <- float haskell
  _delta <- (try (negate <$ char '-') <|> pure id) <*> float haskell
  string ", Omega="
  _bigomega <- (try (negate <$ char '-') <|> pure id) <*> float haskell
  string ", omega="
  _smallomega <- (try (negate <$ char '-') <|> pure id) <*> float haskell
  string ", stop penalty="
  _stopPenalty <- (try (negate <$ char '-') <|> pure id) <*> float haskell
  return $ RNAcode _rnacodeHits (Just (fromInteger _alignmentnumber)) (Just _time) (Just (fromInteger _samples)) (Just _delta) (Just _bigomega) (Just _smallomega) (Just _stopPenalty)

-- | Parse the input as RNAcodeHit
genParseRNAcodeHit :: GenParser Char st RNAcodeHit
genParseRNAcodeHit = do
  many (char ' ')
  _hss <- natural haskell
  _frame <- integer haskell
  _length <- natural haskell
  _from <- natural haskell
  _to <- natural haskell
  --_name <- identifier haskell
  _name <- many1 (oneOf "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz1234567890_-")
  string ("\t")
  _start <- natural haskell
  _end <- natural haskell
  _score <- float haskell
  _pvalue <- many1 (try (choice [digit,char '.']))
  newline
  return $ RNAcodeHit (fromInteger _hss) (fromInteger _frame) (fromInteger _length) (fromInteger _from) (fromInteger _to) _name (fromInteger _start) (fromInteger _end) _score (read _pvalue :: Double)

-- | parse RNAcode from input string
parseRNAcode :: String -> Either ParseError RNAcode
parseRNAcode = parse genParseRNAcode "parseRNAcode"

-- | parse RNAcode from input filePath                      
readRNAcode :: String -> IO (Either ParseError RNAcode)                  
readRNAcode filePath = do 
  parsedFile <- parseFromFile genParseRNAcode filePath
  CE.evaluate parsedFile

-- | parse RNAcode from input string
parseRNAcodeTabular :: String -> Either ParseError RNAcode
parseRNAcodeTabular = parse genParseRNAcodeTabular "parseRNAcode" 

-- | parse RNAcode from input filePath                      
readRNAcodeTabular :: String -> IO (Either ParseError RNAcode)                  
readRNAcodeTabular filePath = do 
  parsedFile <- parseFromFile genParseRNAcodeTabular filePath
  CE.evaluate parsedFile
