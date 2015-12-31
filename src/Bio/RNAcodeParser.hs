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
import qualified Control.Exception.Base as CE

-- | Run external RNAcode command and read the output into the corresponding datatype
systemRNAcode :: String -> String -> String -> IO ExitCode
systemRNAcode options inputFilePath outputFilePath = system ("RNAcode " ++ options ++ " " ++ inputFilePath ++ " >" ++ outputFilePath)

-- | Parse the input as RNAcode datatype
genParseRNAcode :: GenParser Char st RNAcode
genParseRNAcode = do
  many1 (noneOf " ")
  _version <- many1 (noneOf " ")
  return $ RNAcode _version
           
-- | parse RNAcode from input string
parseRNAcode :: [Char] -> Either ParseError RNAcode
parseRNAcode input = parse genParseRNAcode "parseRNAcode" input

-- | parse RNAcode from input filePath                      
readRNAcode :: String -> IO (Either ParseError RNAcode)                  
readRNAcode filePath = do 
  parsedFile <- parseFromFile genParseRNAcode filePath
  CE.evaluate parsedFile
