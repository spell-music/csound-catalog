{-# Language FlexibleInstances #-}
module VestigeOfTime where

import Csound.Base
     
toInstr :: Se a => D -> (Sig -> a) -> (Msg -> a)
toInstr coeff f = \msg -> mapSe (((sig $ coeff * ampmidi msg) * env1) * ) $ f (sig $ cpsmidi msg) 

--midis :: Out a => [Msg -> a] -> GE a
--midis instrs = fmap sum $ sequence $ zipWith midi [1 ..] instrs

main = dac $ do 
    asig <- midi 1 instr12

    return $ eff asig

eff asig = effect ((* 0.3) . (\x -> reverb2 x 2 0.2)) . 
    effect (\x -> 0.7 * asig + 0.5 * x) $ delayLine 6 1.2 0.9 asig

instr = instr3
        
env1 = linsegr [1, 0.5, 1, 1, 1] 1.5 0
env2 = expsegr [1, 0.3, 0.0001] 1 0.0001

instr1 = toInstr 0.3 (filteredSawFalling 10) 
  
instr3 msg = fmap ((sig amp * 0.5 * env1) * ) $ filteredNoise (cycleLine (10 * (1 - amp)) 50 cps) (sig $ 5 + amp * 50)
    where (amp, cps) = ampCps msg

instr4 = toInstr 1.5 femaleA 
instr5 = toInstr 1.5 femaleIY
instr6 = toInstr 1.5 femaleE

instr8 = toInstr 0.25 $ randomPitch (q 0.03) (q 50) (filteredSawFalling 7)
    where q = ( * linseg [0, 2, 1, 1, 1])

instr9 = toInstr 0.5 $ resonInstr (f f21) (f f22) (f f23) 1 
    where f = onceBy 10 

instr10 = toInstr 0.25 $ resonVibrato 10 5 (cycleLine 5 1 0) 1  

instr11 = toInstr 0.5 $ delaySaw 1 -- (onceBy 4 f37) 

instr12 msg = 0.5 * env1 * amBell amp (sig cps)
    where (amp, cps) = ampCps msg

 -- bug in if-exps
 -- branches are forced to k-rate (but they should stay in a-rate)
instr7 msg = do
    q <- fmap sig $ randomD 0 3    
    let a = guardedB true [(q <* 1, a1), (q <* 2, a2)] a3
    return $ sig amp * 1.5 * env1 * a
    where (amp, cps) = ampCps msg
          a1 = femaleA (sig cps)
          a2 = femaleO (sig cps)
          a3 = femaleE (sig cps)

sawTab = sines [1, 0.5, 0.333, 0.25, 0.2, 0.167, 0.1428, 0.125, 0.111, 0.1, 0.0909, 0.0833, 0.0769, 0.0714, 0.0667, 0.0625]        

class Se a where
    fromSe :: a -> SE Sig
    mapSe  :: (Sig -> Sig) -> (a -> a)
    
instance Se Sig      where { fromSe = return ; mapSe = ($)  }
instance Se (SE Sig) where { fromSe = id     ; mapSe = fmap }

chorus :: [Sig] -> (Sig -> Sig) -> Sig -> Sig
chorus ks f = \cps -> mean $ fmap (f . (+ cps)) ks

randomPitch :: Se a => Sig -> Sig -> (Sig -> a) -> (Sig -> SE Sig)
randomPitch rndAmp rndCps f cps = go =<< randh (cps * rndAmp) rndCps
    where go krand = fromSe $ f (cps + krand)

cycleLine :: D -> D -> D -> Sig
cycleLine dt a b = loopseg (sig $ 0.5 / dt) 0 0 [sig a, 0.5, sig b, 0.5, sig a]

onceBy :: D -> Tab -> Sig
onceBy dt tab = oscBy tab (1 / sig dt) 

-- effects    
delayLine :: Int -> D -> D -> Sig -> (Sig, Sig)
delayLine n k dt asig = (mean $ asig : odds asigs, mean $ asig : evens asigs)
    where phi x = delaySig (x * sig k) dt
          asigs = take n $ iterate phi (delaySig asig dt)

odds :: [a] -> [a]
odds as = fmap snd $ filter fst $ zip (cycle [True, False]) as 

evens :: [a] -> [a]
evens as 
    | null as   = []
    | otherwise = odds $ tail as

-- instruments

filteredSawRising :: D -> Sig -> Sig
filteredSawRising riseDur = filteredSaw (linseg [500, riseDur, 5000, 1, 5000])

filteredSawFalling :: D -> Sig -> Sig
filteredSawFalling fallDur = filteredSaw (linseg [5000, fallDur, 500, 1, 500])

filteredSaw :: Sig -> Sig -> Sig
filteredSaw kcf cps = aout
    where 
        a1      = chorus [0, 0.998, 1.003] (oscBy sawTab) cps
        aout    = reson a1 kcf 100 `withD` 2


filteredNoise :: Sig -> Sig -> SE Sig
filteredNoise cfq  bw = do
    anoise <- rand 1
    return $ balance (reson anoise cfq bw `withD` 2) anoise

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

vibrato :: Sig -> Sig -> (Sig -> a) -> (Sig -> a)
vibrato vibDepth vibRate f cps = f (cps + kvib)
    where kvib = vibDepth * kr (osc vibRate) 

resonVibrato :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
resonVibrato vibDepth vibRate filt amp cps = aout
    where
        asig = vibrato vibDepth vibRate ((amp * ) . oscBy waveTab) cps
        aout = reson asig (5000 * filt) 50 `withD` 2

        waveTab = sines [1, 0.832, 0.5, 0.215, 0.6, 0.133, 0.785, 0.326, 0.018, 0.028, 0.0647, 0.0143, 0.0213]

femaleVowel :: [(Sig, Sig)] -> Sig -> Sig
femaleVowel vowel cps = aout
    where 
        afilt1 = chorus [0, 1.003] (\x -> buzz 1 x 15 sine) cps
        aout = blp 2000 $ mean $ fmap (\(a, b) -> reson afilt1 a b `withD` 2) vowel
            
femaleO  = femaleVowel [(280, 20), (650, 25), (2200, 30), (3450, 40), (4500, 50)]
femaleA  = femaleVowel [(650, 50), (1100, 50), (2860, 50), (3300, 50), (4500, 50)] 
femaleE  = femaleVowel [(500, 50), (1750, 50), (2450, 50), (3350, 50), (5000, 50)]
femaleIY = femaleVowel [(330, 50), (2000, 50), (2800, 50), (3650, 50), (5000, 50)]
female02 = femaleVowel [(400, 50), (840, 50), (2800, 50), (3250, 50), (4500, 50)]

femaleVibO :: D -> D -> Sig -> Sig
femaleVibO vibDepth vibRate cps = femaleO vibrato
    where vibrato = cps + sig vibDepth * osc (sig vibRate)

deepFm :: D -> Tab -> Sig -> Sig
deepFm xdur envTab cps = onceBy xdur envTab * aout
    where
        modTab = elins [0, 1]
        mod1 = 0.05 * onceBy xdur modTab * osc cps
        mod2 = 0.07 * osc cps
        aout = osc (cps + mod1 + mod2)


delaySaw :: Sig -> Sig -> Sig
delaySaw amp cps = vibroDelay 6 3 2 0.25 $ amp * 1.5 * femaleE cps  -- oscBy sawTab cps

vibroDelay :: Int -> D -> Sig -> Sig -> Sig -> Sig
vibroDelay order delayBufSize vibDepth vibRate asig = balance aout asig
    where aout = mean $ take order $ iterate del asig
          del x = vdelay x (vibDepth * uosc vibRate) delayBufSize

f37 = splines [0.02, 208, 0.78, 3302, 0.66, 481, 0.22, 105, 0.01]

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

wow :: D -> Sig -> Sig
wow xdur = femaleVowel ks 
    where kmod = onceBy xdur $ skipNorm $ splines [-0.32, 1261, 0.14, 2835, -0.32]
          ks = zip (fmap (+ kmod) [650, 1100, 2860, 3300, 4500]) (repeat 50)

-- tabs

f21 = lins [1.000, 16, 0.950, 17, 0.830, 18, 0.680, 7, 0.530, 11, 0.390, 24, 0.200, 25, 0.120, 28, 0.050, 110, 0.000 ]

f22 = lins [0.000, 20, 0.790, 8, 0.920, 14, 0.980, 14, 0.880, 11, 0.730, 17, 0.580, 17, 0.420, 16, 0.280, 21, 0.210, 19, 0.140, 99, 0.000 ]

f23 = lins [0.000, 46, 0.690, 14, 0.880, 22, 0.980, 17, 0.880, 17, 0.700, 14, 0.570, 19, 0.400, 16, 0.310, 25, 0.220, 30, 0.090, 36, 0.000]


