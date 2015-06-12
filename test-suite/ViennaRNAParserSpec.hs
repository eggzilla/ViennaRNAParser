module ViennaRNAParserSpec (spec) where

import Bio.ViennaRNAParser 
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Parsec.Error
import Text.Parsec.Pos

spec :: Spec
spec = do
  describe "RNAalifoldParser" $ do
    context "Parsing RNAalifold input" $ do
      it "Returns RNAalifold type" $ do
        (parseRNAalifold rnaalifoldexample) `shouldBe` Right rnaalifoldresult
    context "Parsing invalid input" $ do
      it "Returns ParseError" $ do
        (parseRNAalifold "invalid input") `shouldBe` Left rnaalifoldParseError
  describe "RNAdistanceParser" $ do
    context "Parsing RNAdistance input" $ do
      it "Returns RNAdistance type" $ do
        (parseRNAdistance rnadistanceexample) `shouldBe` Right rnadistanceresult
    context "Parsing invalid input" $ do
      it "Returns ParseError" $ do
        (parseRNAdistance "invalid input") `shouldBe` (Left rnadistanceParseError)
  describe "RNAfoldParser" $ do
    context "Parsing RNAfold input" $ do
      it "Returns RNAfold type" $ do
        (parseRNAfold rnafoldexample) `shouldBe` Right rnafoldresult
    context "Parsing invalid input" $ do
      it "Returns ParseError" $ do
        (parseRNAfold "invalid input") `shouldBe` (Left rnafoldParseError)
  describe "RNAplexParser" $ do
    context "Parsing RNAplex input" $ do
      it "Returns RNAplex type" $ do
        (parseRNAplex rnaplexexample) `shouldBe` Right rnaplexresult
    context "Parsing invalid input" $ do
      it "Returns ParseError" $ do
        (parseRNAplex "invalid input") `shouldBe` (Left rnaplexParseError)
  describe "RNAupParser" $ do
    context "Parsing RNAup input" $ do
      it "Returns RNAup type" $ do
        (parseRNAup rnaupexample) `shouldBe` Right rnaupresult
    context "Parsing invalid input" $ do
      it "Returns ParseError" $ do
        (parseRNAup "invalid input") `shouldBe` (Left rnaupParseError)

rnaalifoldParseError :: ParseError
rnaalifoldParseError = addErrorMessage (Expect "lf new-line") (addErrorMessage (SysUnExpect "") (newErrorMessage rnaalifoldErrorMessage rnaalifoldErrorSourcePosition))

rnaalifoldErrorMessage :: Message
rnaalifoldErrorMessage = SysUnExpect ""

rnaalifoldErrorSourcePosition :: SourcePos
rnaalifoldErrorSourcePosition = newPos "genParseRNAalifold" 1 14

rnaalifoldexample :: String
rnaalifoldexample = "__UGGUGGC__CAUAGC_GGA_G_G__G_G_AAA_C_ACCCGU_U__C_CCAUUCCGAACACGGAAGUUAA_GC_CCUUCAGC___G___C___C____G___A___UG___G___UACUGCGGGGG___AA___C________GGCGUG_GGA_G_A_GU_A__G_G_UCGCCGCC_A_G___\n.((((((((......((.(((.(.(..............((((.................))))...........))))).))...................................(((((((...................)))))).)...................)))))).).)... (-28.28 = -23.14 +  -5.14)\n"

rnaalifoldresult :: RNAalifoldOutput
rnaalifoldresult = RNAalifoldOutput "__UGGUGGC__CAUAGC_GGA_G_G__G_G_AAA_C_ACCCGU_U__C_CCAUUCCGAACACGGAAGUUAA_GC_CCUUCAGC___G___C___C____G___A___UG___G___UACUGCGGGGG___AA___C________GGCGUG_GGA_G_A_GU_A__G_G_UCGCCGCC_A_G___" ".((((((((......((.(((.(.(..............((((.................))))...........))))).))...................................(((((((...................)))))).)...................)))))).).)..." (-28.28) (-23.14) (-5.14) 

rnadistanceexample :: String
rnadistanceexample = "f: 96"

rnadistanceresult :: RNAdistance
rnadistanceresult = RNAdistance 96

rnadistanceParseError :: ParseError
rnadistanceParseError = addErrorMessage (Expect "\"f: \"") (newErrorMessage rnadistanceErrorMessage rnadistanceErrorSourcePosition)

rnadistanceErrorMessage :: Message
rnadistanceErrorMessage = SysUnExpect "\"i\""

rnadistanceErrorSourcePosition :: SourcePos
rnadistanceErrorSourcePosition = newPos "genParseRNAdistance" 1 1

rnafoldexample :: String
rnafoldexample = ">AB001721.1/2735-2851\nCCCGGUGACUAUAGAGAGAGGGCCACACCCGUUCCCAUCCCGAACACGGAAGUUAAGCCUCUCAUCGCUGAUGGUACUAUGUGGUUCGCUGCAUGGGAGAGUAGGACGUUGCCGGGU\n(((((((((....(.(((.(((.....))).))).)...(((....)))..(((..(((((((((.((.(.(((.(((....))))))).))))))))).))..)))))))))))). (-38.30)"

rnafoldresult :: RNAfold
rnafoldresult = RNAfold "AB001721.1/2735-2851" "CCCGGUGACUAUAGAGAGAGGGCCACACCCGUUCCCAUCCCGAACACGGAAGUUAAGCCUCUCAUCGCUGAUGGUACUAUGUGGUUCGCUGCAUGGGAGAGUAGGACGUUGCCGGGU" "(((((((((....(.(((.(((.....))).))).)...(((....)))..(((..(((((((((.((.(.(((.(((....))))))).))))))))).))..))))))))))))." (-38.30)

rnafoldParseError :: ParseError
rnafoldParseError = addErrorMessage (Expect "\">\"") (newErrorMessage rnafoldErrorMessage rnafoldErrorSourcePosition)

rnafoldErrorMessage :: Message
rnafoldErrorMessage = SysUnExpect "\"i\""

rnafoldErrorSourcePosition :: SourcePos
rnafoldErrorSourcePosition = newPos "genParseRNAfold" 1 1

rnaplexexample :: String
rnaplexexample = ">1\n>0\n((((((((..(((((((((((((..((((((&)))))).)))))))))))))..))))))))  22,52  :  23,52  (-22.34 = -33.37 +  4.77 +  6.26)\n"

rnaplexresult :: [RNAplexInteraction]
rnaplexresult = [RNAplexInteraction "1" "0" "((((((((..(((((((((((((..((((((&)))))).)))))))))))))..))))))))" 22 52 23 52 (-22.34) (Just (-33.37)) (Just (4.77)) (Just 6.26)]

rnaplexParseError :: ParseError
rnaplexParseError = addErrorMessage (Expect "\">\"") (newErrorMessage rnaplexErrorMessage rnaplexErrorSourcePosition)

rnaplexErrorMessage :: Message
rnaplexErrorMessage = SysUnExpect "\"i\""

rnaplexErrorSourcePosition :: SourcePos
rnaplexErrorSourcePosition = newPos "parseRNAplexOutput" 1 1

rnaupexample :: String
rnaupexample = ">13\n>130\n(((((((.((((((((((.((((&)))).))))).))).)).))))))) 819,841 : 189,213 (-24.35 = -35.30 + 10.95)\nGGCGGCGCUGGGUGGCGAAAUCG&CGAUAUCGCCUACCACACUGCCGCC\n"

rnaupresult :: [RNAupInteraction]
rnaupresult = [RNAupInteraction "13" "130" [(RNAupInteractionRegion "(((((((.((((((((((.((((&)))).))))).))).)).)))))))" 819 841 189 213 (-24.35) (Just (-35.30)) (Just 10.95) Nothing "GGCGGCGCUGGGUGGCGAAAUCG" "CGAUAUCGCCUACCACACUGCCGCC" Nothing)]]

rnaupParseError :: ParseError
rnaupParseError = addErrorMessage (Expect "\">\"") (newErrorMessage rnaupErrorMessage rnaupErrorSourcePosition)

rnaupErrorMessage :: Message
rnaupErrorMessage = SysUnExpect "\"i\""

rnaupErrorSourcePosition :: SourcePos
rnaupErrorSourcePosition = newPos "parseRNAupOutput" 1 1

rnaZexample :: String
rnaZexample = "############################  RNAz 2.1  ##############################\n\n\n Sequences: 4\n Columns: 73\n Reading direction: forward\n Mean pairwise identity:  80.82\n Shannon entropy: 0.31118\n G+C content: 0.54795\n Mean single sequence MFE: -27.20\n Consensus MFE: -26.50\n Energy contribution: -23.63\n Covariance contribution:  -2.87\n Combinations/Pair:   1.43\n Mean z-score:  -1.82\n Structure conservation index:   0.97\n Background model: dinucleotide\n Decision model: sequence based alignment quality\n SVM decision value:   2.15\n SVM RNA-class probability: 0.984068\n Prediction: RNA\n\n######################################################################\n\n>sacCer1.chr4 1352453 73 - 1531914\nGCCUUGUUGGCGCAAUCGGUAGCGCGUAUGACUCUUAAUCAUAAGGUUAGGGGUUCGAGCCCCCUACAGGGCU\n(((((((.(((((........))))...((((.((((....))))))))(((((....)))))).))))))). ( -29.20, z-score =  -2.35, R)\n>sacBay.contig_465 14962 73 - 57401\nGCCUUGUUGGCGCAAUCGGUAGCGCGUAUGACUCUUAAUCAUAAGGUUAGGGGUUCGAGCCCCCUACAGGGCU\n(((((((.(((((........))))...((((.((((....))))))))(((((....)))))).))))))). ( -29.20, z-score =  -2.35, R)\n>consensus\nGCCUUGUUGGCGCAAUCGGUAGCGCGUAUGACUCUUAAUCAUAAGGUUAGGGGUUCGAGCCCCCUACAGGGCU\n(((((((..((((........)))).(((((.......))))).....(((((.......)))))))))))). (-26.50 = -23.63 +  -2.87)\n"

rnaZresult :: RNAzOutput
rnaZresult = RNAzOutput "RNAz 2.1" 4 73 "forward" 80.82 0.31118 0.54795 (-27.20) (-26.50) (-23.63) (-2.87) 1.43 (-1.82) 0.97 "dinucleotide" "sequence based alignment quality" 2.15 0.984068 "RNA" rnaZEntries rnaZconsenus

rnaZEntries :: [RNAzResult]
rnaZEntries = [RNAzResult "sacCer1.chr4 1352453 73 - 1531914" "GCCUUGUUGGCGCAAUCGGUAGCGCGUAUGACUCUUAAUCAUAAGGUUAGGGGUUCGAGCCCCCUACAGGGCU" "(((((((.(((((........))))...((((.((((....))))))))(((((....)))))).)))))))." (-29.20) (-2.35) 'R',RNAzResult "sacBay.contig_465 14962 73 - 57401" "GCCUUGUUGGCGCAAUCGGUAGCGCGUAUGACUCUUAAUCAUAAGGUUAGGGGUUCGAGCCCCCUACAGGGCU" "(((((((.(((((........))))...((((.((((....))))))))(((((....)))))).)))))))." (-29.20) (-2.35) 'R']

rnaZconsenus :: RNAzConsensus
rnaZconsenus = RNAzConsensus "GCCUUGUUGGCGCAAUCGGUAGCGCGUAUGACUCUUAAUCAUAAGGUUAGGGGUUCGAGCCCCCUACAGGGCU" "(((((((.(((((........))))...((((.((((....))))))))(((((....)))))).)))))))."

rnaZParseError :: ParseError
rnaZParseError = addErrorMessage (Expect "\">\"") (newErrorMessage rnaupErrorMessage rnaupErrorSourcePosition)

rnaZErrorMessage :: Message
rnaZErrorMessage = SysUnExpect "\"i\""

rnaZErrorSourcePosition :: SourcePos
rnaZErrorSourcePosition = newPos "parseRNAupOutput" 1 1
