module Scales where

import Control.Lens
import Euterpea
  
type Degree = Int

data TaggedNote = TaggedNote
  {
    getAbs   :: AbsPitch,
    degree   :: Degree
  } deriving (Eq, Ord, Show, Read)

ppNote :: TaggedNote -> String
ppNote n = " pitch:   " ++ show (pitch (getAbs n)) ++
           " degree:  " ++ show (degree n) ++ "\n"

type PitchCollection = [TaggedNote]

data Scale = Scale
  {
    scaleRoot       :: TaggedNote,
    scaleNotes      :: PitchCollection
  } deriving (Eq, Show, Read)

ppScale :: Scale -> String
ppScale s = concatMap ppNote (scaleNotes s)

transposeNoteBy :: Int -> TaggedNote -> TaggedNote
transposeNoteBy n orig = TaggedNote
                         {
                           getAbs = (getAbs orig) + n,
                           degree = degree orig
                         }
  
transposeScaleBy :: Int -> Scale -> Scale
transposeScaleBy n original = Scale
  {
    scaleRoot  = transposeNoteBy n $ scaleRoot original,
    scaleNotes = map (transposeNoteBy n) (scaleNotes original)  
  }
  
makeScale :: AbsPitch -> Mode -> Scale
makeScale root mode = transposeScaleBy root $ Scale
  {
    scaleRoot = TaggedNote
                {
                  getAbs = root,
                  degree = 1
                },
    scaleNotes = zipWith TaggedNote (scale mode) [1..]
  }
  
                                   

nthDegree :: Mode -> AbsPitch -> Degree -> TaggedNote
nthDegree mode root deg
  | deg <= 7  = TaggedNote
                {
                  getAbs = chordTones,
                  degree = deg
                }
  | otherwise = TaggedNote
                {
                  getAbs = upperExts,
                  degree = deg
                }
  where
    chordTones = root + ((scale mode) !! (deg - 1))
    upperExts  = root + 12 + ((scale mode) !! (deg `mod` 8)) 


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
  
ion  = [0,2,4,5,7,9,11]
dor  = [0, 2, 3, 5, 7, 9, 10]
phr  = [0, 1, 3, 5, 7, 8, 10]
lyd  = [0, 2, 4, 6, 7, 9, 11]
mixo = [0, 2, 4, 5, 7, 9, 10]
aeo  = [0,2,3,5,7,8,10]
loc  = [0, 1, 3, 5, 6, 8, 10]
