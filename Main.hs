module Main where
import Euterpea
import HSoM.Examples.EuterpeaExamples
import NegativeHarmony

testMel :: Music Pitch
testMel = line $ map ($qn) [c 4, e 4, g 4, b 4,
                            a 4, f 4, d 4, f 4,
                            e 4, e 4, f 4, f 4, g 4]

main :: IO ()
main = do
  putStrLn "Test melody:"
  playDev 2 testMel
  putStrLn "Test melody negated:"
  playDev 2 (negateHarm (C, 4) testMel)
  return ()
