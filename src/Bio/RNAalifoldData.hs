-- | This module contains a data structure for RNAalifold output
--   For more information on RNAalifold consult: <>

module Bio.RNAalifoldData where
data RNAalifoldOutput = RNAalifoldOutput
  {
    sequenceNumber :: Int,
    alignmentLength :: Int,
    alignmentConsensusSequence :: String,
    alignmentConsensusDotBracket :: String,
    alignmentConsensusMinimumFreeEnergy :: Double,
    alignmentConsensusInitialMinimumFreeEnergy :: Double,
    alignmentConsensusEnergyCovarianceContributions :: Double
  }
  deriving (Show, Eq)
