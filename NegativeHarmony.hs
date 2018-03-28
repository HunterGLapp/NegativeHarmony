module NegativeHarmony where
import Euterpea

negateHarm :: Pitch -> Music Pitch -> Music Pitch
negateHarm tonic (Prim pr)       = Prim (negatePr tonic pr)
negateHarm tonic (m1 :+: m2)     = (negateHarm tonic m1) :+:
                                   (negateHarm tonic m2)
negateHarm tonic (m1 :=: m2)     = (negateHarm tonic m1) :=:
                                   (negateHarm tonic m2)
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
                      
