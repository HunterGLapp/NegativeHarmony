module NegativeHarmony where
import Euterpea

negatePitchAround :: Pitch -> Pitch -> Pitch
negatePitchAround tonic p = reflect p (getPivots tonic)

getPivots :: Pitch -> AbsPitch
getPivots  p = absPitch p + 3

reflect :: Pitch -> AbsPitch -> Pitch
reflect orig pivot = pitch $ pivot
                             - (absPitch orig - (pivot + 1))
                      
