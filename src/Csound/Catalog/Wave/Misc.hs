module Csound.Catalog.Wave.Misc (
    okComputer, polySynthFx, polySynth,
    dreamPad, underwaterPad, lightIsTooBrightPad, whaleSongPad,
    deepBass
) where

import Csound.Base 

-- | Tech sound. Random sinusoids palyed at the very fast rate. 
--
-- > okComputer rate
--
-- * @rate@ -- rate of new notes ~ (5, 20)
okComputer :: Sig -> SE Sig
okComputer cps = fmap go $ noise 11000 0.99
    where
        go anoise = osc (samphold anoise kgate)
        kgate = kr $ oscil 1 cps (elins [1, 0, 0])


polySynth x = mul (fades 0.01 0.15) $ at (mlp 5500 0.12) $ at (filt 2 br 18000 0.3) $ uni rndSaw x + uni rndSaw (x * cent 14) + (mul 0.2 $ at (lp 400 0.1) white)
   where uni = multiHz 2 (cent 50)

polySynthFx :: SE Sig -> SE Sig2
polySynthFx = mixAt 0.25 largeHall2 . mixAt 0.25 (echo 0.25 0.65) . at (chorus 0.07 1.25 0.25) . at fromMono

uni = multiHz 2 (cent 50)

dreamPad = genDreamPadInstr mkOsc 
    where mkOsc vibLfo1 vibLfo2 x = uni rndSaw (vibLfo1 x) + uni rndSaw (vibLfo2 $ x * cent 14) 

underwaterPad = genDreamPadInstr mkOsc 
    where mkOsc vibLfo1 vibLfo2 x = uni rndTri (vibLfo1 x) + uni rndTri (vibLfo2 $ x * cent 14) 

lightIsTooBrightPad = genDreamPadInstr mkOsc 
    where mkOsc vibLfo1 vibLfo2 x = uni rndSaw (vibLfo1 x) + uni rndSaw (vibLfo2 $ x * cent 14) + mul 0.3 (mul (uosc 0.25) (rndTri (vibLfo2 $ x * 7 * cent 4)) + mul (isawSeq [1, 0.5, 0.25] 6 * uosc 0.17) (rndTri (vibLfo2 $ x * 13)) + mul (sqrSeq [1, 0.5, 0.25, 0.1] 8 * uosc 0.28) (rndOsc (vibLfo2 $ x * 9 * cent 3)))

whaleSongPad = genDreamPadInstr mkOsc 
    where mkOsc vibLfo1 vibLfo2 x = uni rndTri (vibLfo1 x) + uni rndTri (vibLfo2 $ x * cent 14) + uni rndTri (vibLfo2 $ 3 * x * cent 14) + mul 0.15 (uni rndTri (vibLfo2 $ 7 * x * cent 14)) + mul 0.15 (uni rndTri ((vibLfo2 $ 11 * x * cent 14) + 400 * uosc 0.2))

genDreamPadInstr mkOsc brightness x = do
    a1 <- oscs
    a2 <- nois
    return $ mul (fades 0.85 0.95) $ fx1 (a2 + a1) + fx2 a1   
    where
        fx1 = filt 2 mlp (filtLfo1 (700 + brightness * 2500)) 0.26
        fx2 = mlp (filtLfo2 (1200 + brightness * 2500)) 0.32

        -- saw
        oscs = mkOsc vibLfo1 vibLfo2 x

        -- underwater
        -- oscs = uni rndTri (vibLfo1 x) + uni rndTri (vibLfo2 $ x * cent 14)  -- + uni rndTri (vibLfo2 $ 3 * x * cent 14)

        -- wales howling
        -- oscs = uni rndTri (vibLfo1 x) + uni rndTri (vibLfo2 $ x * cent 14) + uni rndTri (vibLfo2 $ 3 * x * cent 14) + mul 0.15 (uni rndTri (vibLfo2 $ 7 * x * cent 14)) + mul 0.15 (uni rndTri ((vibLfo2 $ 11 * x * cent 14) + 400 * uosc 0.2))
        nois = mul 0.35 $ at (lp 2400 0.1) white

        uni = multiHz 2 (cent 50)
        lfo1 y x = x * (1 + y * osc (0.35 + 0.05  * osc 0.1))
        lfo2 y x = x * (1 + y * osc (0.22 + 0.043 * osc 0.14))

        filtLfo1 = lfo1 0.18
        filtLfo2 = lfo2 0.13

        vibLfo1 = lfo1 0.005
        vibLfo2 = lfo2 0.007

deepBass x = mul 0.5 $ at (hp1 45) $ at (\x -> dam x 0.45 2 2 0.01 0.01) $  mul (xeg 0.005 0.6 1 0.05) $ sum [(filt 2 lp 275 0.25) (saw $ x * 0.5), osc (x * 0.5)]
