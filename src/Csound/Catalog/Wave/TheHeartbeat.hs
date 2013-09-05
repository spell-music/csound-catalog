module Csound.Catalog.Wave.TheHeartbeat (
    crackle, heartbeat, monoPluck, chorusel
) where

import Csound.Base

-- | 
-- > crackle noteDuration attackTime decayTime cps
crackle :: D -> D -> D -> Sig -> Sig
crackle xdur att dec cps = a3 
    where a1 = k1 * osc cps
          k1 = linen 1 att xdur dec
          a2 = fof a1 (a1 + cps) (a1 / 50) k1 200 0.003 0.017 0.005 20 f1 f2 xdur
          arev = reverb2 a2 5 1
          a3 = 0.2 * (a2 + arev) 
          f1 = sines [1] 								            -- SINE WAVE
          f2 = sines [1, 0.5, 0.3, 0.25, 0.2, 0.167, 0.14, 0.111] 	-- SAWTOOTH

-- | Deep kick sound.
heartbeat :: Sig
heartbeat = phi 0.0024 f12 + phi 0.0078 f13
    where phi dec ftab = oscili (linseg [1, xdur, dec, 1, dec]) 4 ftab
          f12 = sines2 [(10, 1), (16, 1.5), (22, 2), (23, 1.5)]
          f13 = sines2 [(25, 1), (29, 0.5), (32, 0.2)] 
          xdur = 0.25

-- | 
-- > monoPluck xdur pick plk amplitude cps
--
-- * pick - Proportion of the way along the string to sample the output.
-- 
-- * plk - The point of pluck is iplk, which is a fraction of the way up the string (0 to 1). A pluck point of zero means no initial pluck.
monoPluck :: D -> D -> D -> D -> D -> Sig
monoPluck xdur pick plk amp cps = a3  
    where repluck' freq a = repluck plk (sig amp) freq (sig pick) 0.5 a           
          a1 = mean 
                [ repluck' (cps - 1) (osc $ sig $ cps - 2) 
                , repluck' (cps + 1) (osc $ sig $ cps + 2) ]
          a2 = linen (a1/2) (0.2 * xdur) xdur (0.8 * xdur)
          arev = reverb2 a2 1.5 1
          a3 = (a2 + 0.6 * arev) / 1.6
         
-- |
-- > chorusel dur rise dec cps
-- 
-- * dur - note duration
-- 
-- * rise - rise time
--
-- * dec - decay time
--
-- * cps - frequency of the note
chorusel :: D -> D -> D -> Sig -> (Sig, Sig)
chorusel xdur rise dec cps = (a1, a2)
    where k1 = linen 1 rise xdur dec
          k2 = linseg [1, idur, 0]
          k3 = kr $ osc 2
          k4 = kr $ 0.5 * osc 2
          inote = cpspch cps
          as = fmap (\(d, a, f) -> k1 * f (inote + d + a)) [
            (-1, k3, saw),
            (1,  k4, f9),
            (-0.5, 0, f9),
            (0.5, 0, saw),
            (-2, k4, saw),
            (2, k3, f9),
            (-1.5, k3, saw),
            (1.5, k3, f9),
            (-0.25, 0, f9),
            (0.25, 0, saw),
            (-0.8, k4, saw),
            (0.8, k4, f9)]
            
          ars = zipWith3 (\a k d -> withInits (areson a k d) (1::D)) as (k2:(k2*k3):repeat k2) (fmap (sig . double) $ [10, 20 .. 80] ++ [50, 60 .. 80])
          meanArs = (/ 5.5) . sum . fmap (ars !!) 
          asig1 = meanArs [0, 3, 5, 7, 8]
          asig2 = meanArs [1, 2, 4, 6, 9]
          asig3 = 0.5 * (ars !! 10 + ars !! 11)

          a1 = 0.5 * (asig1 + asig3)
          a2 = 0.5 * (asig2 + asig3)          
                    
          f9 phs = oscil 1 phs $ sines 
                        [ 0.28, 1, 0.74, 0.66, 0.78, 0.48, 0.05, 0.33, 0.12
                        , 0.08, 0.01, 0.54, 0.19, 0.08, 0.05, 0.16, 0.01
                        , 0.11, 0.3, 0.02, 0.2] 

