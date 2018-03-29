module NegativeHarmony where
import Euterpea
import Scales

negMode :: Mode -> Mode
negMode m = case m of
                Major      -> Minor
                Minor      -> Major
                Ionian     -> Aeolian
                Dorian     -> Mixolydian
                Phrygian   -> Lydian
                Lydian     -> Phrygian
                Mixolydian -> Dorian
                Aeolian    -> Ionian
                Locrian    -> Locrian

-- I   ->  VI   aeolian
-- II  ->  V    mixolydian
-- III ->  IV   lydian
-- IV  ->  III  Phrygian
-- V   ->  II   Dorian
-- VI  ->  I    Ionian
-- VII ->  VII  Locrian
negateKey :: Control -> Control -- TODO
negateKey = id


{-
negateKey (KeySig key mode) = keysig newTonic newMode where
  newMode  = negMode mode
  newTonic =
-}
                

-- | negateHarm takes a pitch to use as tonal center and a (Music Pitch)
--   and negates that music, recursing inwards. When a Control "annotation"
--   indicating a key signature is encountered, we use it to determine a new
--   tonal center, to determine an axis around which to reflect and update the
--   modification to reflect mode and tonic changes

negateHarm :: Pitch        -- ^ Initial tonal center
           -> Music Pitch  -- ^ Music to be transformed
           -> Music Pitch
           
negateHarm tonic (Prim pr)                   = Prim (negatePr tonic pr)
negateHarm tonic (m1 :+: m2)                 = (negateHarm tonic m1) :+:
                                               (negateHarm tonic m2)
negateHarm tonic (m1 :=: m2)                 = (negateHarm tonic m1) :=:
                                               (negateHarm tonic m2)
negateHarm oldTonic (Modify (KeySig newTonic mode) m) =
  Modify newKey (negateHarm (newTonic, octave) m) where
                     octave = snd oldTonic
                     newKey = negateKey (KeySig newTonic mode)
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
                      
