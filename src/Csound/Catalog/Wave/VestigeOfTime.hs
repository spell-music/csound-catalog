{-# Language FlexibleInstances #-}
module Csound.Catalog.Wave.VestigeOfTime (
    filteredSaw, filteredSawRising, filteredSawFalling,
    filteredNoise, 
    resonInstr, resonVibrato, 
    delaySaw, femaleVowel, amBell
) where

import Csound.Base
    
import Csound.Catalog.Effect(vibroDelay)
import Csound.Catalog.Reson(Reson)

-- instruments

-- | Filtered saw with rising envelope. Centere frequency starts at 500 Hz
-- and then rises to 5000 by @riseDur@ seconds.
--
-- > filteredSawRising riseDur cps
filteredSawRising :: D -> Sig -> Sig
filteredSawRising riseDur = filteredSaw (linseg [500, riseDur, 5000])

-- | Filtered saw with falling envelope. Centere frequency starts at 5000 Hz
-- and then falls down to 500 by @riseDur@ seconds.
--
-- > filteredSawFalling riseDur cps
filteredSawFalling :: D -> Sig -> Sig
filteredSawFalling fallDur = filteredSaw (linseg [5000, fallDur, 500])

-- | The saw is filtered with band pass filter. Centere frequency of the filter 
-- can vary.
--
-- > filteredSaw centerFrequency sawCps
filteredSaw :: Sig -> Sig -> Sig
filteredSaw kcf cps = aout
    where 
        a1      = chorus [0, 0.998, 1.003] saw cps
        aout    = reson a1 kcf 100 `withD` 2

-- | The white noise is filtered with band pass filter. Centere frequency of the filter 
-- can vary.
--
-- > filteredNoise centerFrequency sawCps
filteredNoise :: Sig -> Sig -> SE Sig
filteredNoise cfq  bw = do
    anoise <- rand 1
    return $ balance (reson anoise cfq bw `withD` 2) anoise

-- | Signal is passed through three band-pass filters. 
-- We can alter the relative center frequencies of the filters.
--
-- > resonInstr filt1 filt2 filt3 amp cps = aout
resonInstr :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
resonInstr filt1 filt2 filt3 amp cps = aout
    where
        asig    = amp * oscBy f19 cps
        asig2   = amp * 0.7 * osc cps

        phi cf bw filt = reson asig (cf * filt) bw `withD` 2
        aout = balance (sum 
                            [ 0.6 * phi 110 20 filt1
                            ,       phi 220 30 filt2
                            , 0.6 * phi 440 40 filt3 
                            , 0.4 * asig 
                            , 2 * asig2 ])
                        asig2
            
        f19 = sines [1, 0.1, 0.1, 0.278, 0.245, 0.3, 0.352, 0.846, 0.669, 0, 0, 0, 0.1, 0.1, 0.1]

-- | Vibrato and resonant filter with varying center frequency.
--
-- > resonVibrato vibDepth vibRate filtCps amp cps = aout
resonVibrato :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
resonVibrato vibDepth vibRate filt amp cps = aout
    where
        asig = vibrato vibDepth vibRate ((amp * ) . oscBy waveTab) cps
        aout = reson asig (5000 * filt) 50 `withD` 2

        waveTab = sines [1, 0.832, 0.5, 0.215, 0.6, 0.133, 0.785, 0.326, 0.018, 0.028, 0.0647, 0.0143, 0.0213]

-- | Singing a reson's vowels (see "Csound.Catalog.Reson"). 
femaleVowel :: Reson -> Sig -> Sig
femaleVowel vowel cps = aout
    where 
        afilt1 = chorus [0, 1.003] (\x -> buzz 1 x 15 sine) cps
        aout = blp 2000 $ resonsBy (\cf bw x -> reson x cf bw `withD` 2) vowel afilt1
          
-- | Delayed saw wave.
delaySaw :: Sig -> Sig
delaySaw cps = vibroDelay 6 3 2 0.25 $ saw cps

-- | Detuned bell.
--
-- > amBell amp cps
amBell :: D -> Sig -> Sig
amBell amp cps = kenv * aout
    where
        phi a b c = (a + b) * osc (c + cps)
        a5      = phi 0.25 0 1729
        a4      = phi 0.3  a5 973
        a1      = phi 0.5  a4 513
        a2      = phi 1    a1 0
        aout    = balance a2 (osc 440)

        kenv    = sig amp * expseg [0.0001, 0.01, 1,amp * 0.3, 0.6, amp * 8, 0.0001]
