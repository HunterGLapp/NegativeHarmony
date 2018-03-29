module Scales where
import Euterpea

ion  = [0,2,4,5,7,9,11]
dor  = [0, 2, 3, 5, 7, 9, 10]
phr  = [0, 1, 3, 5, 7, 8, 10]
lyd  = [0, 2, 4, 6, 7, 9, 11]
mixo = [0, 2, 4, 5, 7, 9, 10]
aeo  = [0,2,3,5,7,8,10]
loc  = [0, 1, 3, 5, 6, 8, 10]

scale :: Mode -> [AbsPitch]
scale mode = case mode of
  Major      -> ion
  Minor      -> aeo
  Ionian     -> ion
  Dorian     -> dor
  Phrygian   -> phr
  Lydian     -> lyd
  Mixolydian -> mixo
  Aeolian    -> aeo
  Locrian    -> loc
  m          -> error ("(getScale) Scale not defined for mode" ++ show m)
