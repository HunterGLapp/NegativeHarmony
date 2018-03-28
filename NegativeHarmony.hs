module NegativeHarmony where
import Euterpea

negModeOf :: Mode -> Mode
negModeOf = id -- TODO : modes change when you negate. eg Ionian -> Phrygian

-- | negateHarm takes a pitch to use as tonal center and a (Music Pitch)
--   and negates that music, recursing inwards. When a Control "annotation"
--   indicating a key signature is encountered, we use it to determine a new
--   tonal center, and transform the modification to reflect a mode change

negateHarm :: Pitch        -- ^ Tonal center (just picks octave if keys present)
           -> Music Pitch  -- ^ Music to be transformed
           -> Music Pitch
           
negateHarm tonic (Prim pr)                   = Prim (negatePr tonic pr)
negateHarm tonic (m1 :+: m2)                 = (negateHarm tonic m1) :+:
                                               (negateHarm tonic m2)
negateHarm tonic (m1 :=: m2)                 = (negateHarm tonic m1) :=:
                                               (negateHarm tonic m2)
negateHarm tonic (Modify (KeySig pc mode) m) =
  Modify (KeySig pc (negModeOf mode)) (negateHarm newTonic m) where
  newTonic = (pc, snd tonic) -- just keeps tonic in same octave
                             -- TODO : move to closest octave
negateHarm tonic (Modify ctrl m) = Modify ctrl (negateHarm tonic m)

negatePr :: Pitch ->
            Primitive Pitch ->
            Primitive Pitch            
negatePr _     (Rest dur)       = Rest dur
negatePr tonic (Note dur pitch) = Note dur (negateP tonic pitch)


negateP :: Pitch -> Pitch -> Pitch
negateP tonic p = reflectP p (getPivot tonic)

getPivot :: Pitch -> AbsPitch -- just returns the minor third
getPivot  p = absPitch p + 3  -- actual pivot is b/w m3 and M3
                             
              
reflectP :: Pitch -> AbsPitch -> Pitch
reflectP orig pivot = pitch $ pivot
                             - (absPitch orig - (pivot + 1))
                      
