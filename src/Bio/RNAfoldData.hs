-- | This module contains a hierarchical data structure for
--   RNAplex output
--   For more information on RNAplex consult: <http://www.bioinf.uni-leipzig.de/Software/RNAplex/>

module Bio.RNAfoldData where
-- | Data structure for individual interaction between a target and query nucleic acid seqence
data RNAfold = RNAfold
  { 
    sequenceIdentifier :: String,
    foldSequence :: String,
    foldSecondaryStructure :: String,
    foldingEnergy :: Double
  }
  deriving (Show, Eq)
