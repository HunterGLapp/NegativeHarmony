{-# LANGUAGE TemplateHaskell #-}

module Scales where

import Control.Lens
import Euterpea
import Data.List(elemIndex)
  
type Degree = Int

data TaggedNote = TaggedNote
  {
    _getAbs   :: AbsPitch,
    _degree   :: Degree
  } deriving (Eq, Ord, Show, Read)

makeLenses ''TaggedNote

ppNote :: TaggedNote -> String
ppNote n =
  " pitch:   " ++ show (pitch (_getAbs n)) ++
  " degree:  " ++ show (_degree n) ++ "\n"

type PitchCollection = [TaggedNote]

ppCollection :: PitchCollection -> String

ppCollection = concatMap ppNote

alterNthDeg :: (AbsPitch -> AbsPitch) ->
               Degree ->
               PitchCollection ->
               PitchCollection
               
alterNthDeg fn deg old =
  case elemIndex deg (map _degree old) of
    Just i  -> fst splitList ++
               [TaggedNote (fn $ _getAbs (old !! i)) deg] ++
               tail (snd splitList) where
      splitList = splitAt (i - 1) old
    Nothing -> undefined 
               
data Scale = Scale
  {
    _scaleRoot       :: PitchClass,
    _scaleNotes      :: PitchCollection
  } deriving (Eq, Show, Read)

makeLenses ''Scale

ppScale :: Scale -> String
ppScale = ppCollection . (view scaleNotes)

transposeNoteBy :: Int -> TaggedNote -> TaggedNote
transposeNoteBy n = over getAbs (+ n)

transposeRootBy :: Int -> PitchClass -> PitchClass
transposeRootBy n pc = fst $ pitch $ absPitch (pc, 0) + 1 -- ugly
  
transposeScaleBy :: Int -> Scale -> Scale
transposeScaleBy n =
  over scaleRoot  (transposeRootBy n) .
  over scaleNotes (map (transposeNoteBy n)) 
  
makeScale :: AbsPitch -> Mode -> Scale
makeScale root mode = transposeScaleBy root $ Scale
  {
    _scaleRoot = fst $ pitch root,
    _scaleNotes = zipWith TaggedNote (scale mode) [1..]
  }
  
                                   

nthDegree :: Mode -> AbsPitch -> Degree -> TaggedNote
nthDegree mode root deg
  | deg <= 7  = TaggedNote
                {
                  _getAbs = chordTones,
                  _degree = deg
                }
  | otherwise = TaggedNote
                {
                  _getAbs = upperExts,
                  _degree = deg
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
  m          -> error ("(scale) Scale not defined for mode" ++ show m)
  
ion  = [0,2,4,5,7,9,11]
dor  = [0, 2, 3, 5, 7, 9, 10]
phr  = [0, 1, 3, 5, 7, 8, 10]
lyd  = [0, 2, 4, 6, 7, 9, 11]
mixo = [0, 2, 4, 5, 7, 9, 10]
aeo  = [0,2,3,5,7,8,10]
loc  = [0, 1, 3, 5, 6, 8, 10]

cLyd = makeScale 12 Lydian
