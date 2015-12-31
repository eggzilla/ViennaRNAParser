-- | This module contains a hierarchical data structure for
--   RNAcode output
--   For more information on RNAcode consult: <http://wash.github.io/rnacode/>

module Bio.RNAcodeData where
    
-- | All elements of RNAcode output are contained in this datatype 
data RNAcode = RNAcode
  { 
    rnacode :: String
  }
  deriving (Show, Eq)
