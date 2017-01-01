module Csound.Catalog.Wave.Misc (
    SubSyntSpec(..), SubSynt, SubSyntKey, SubSynt2, SubSyntKey2,
    okComputer, polySynthFx, polySynth,
    dreamPad, underwaterPad, lightIsTooBrightPad, whaleSongPad,
    dreamPadBy, lightIsTooBrightPadBy, whaleSongPadBy,
    deepBass,

    impulseMarimba1, impulseMarimba2
) where

import Csound.Base 

-- | Subtractive syntesizer specs.
-- 
-- Arguments are: main oscillator, resonant filter, brightness amount.
data SubSyntSpec a = SubSyntSpec
    { subOsc    :: a
    , subFilter :: Sig -> Sig -> Sig -> Sig
    , subBright :: Sig } 

type SubSynt        = SubSyntSpec (Sig -> SE Sig)
type SubSyntKey     = SubSyntSpec (D -> Sig -> SE Sig)
type SubSynt2       = SubSyntSpec (Sig -> SE Sig2)
type SubSyntKey2    = SubSyntSpec (D -> Sig -> SE Sig2)

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

dreamPad brightness = dreamPadBy (SubSyntSpec rndSaw mlp brightness)
underwaterPad brightness = dreamPadBy (SubSyntSpec rndTri mlp brightness)
whaleSongPad brightness = whaleSongPadBy (SubSyntSpec rndTri mlp brightness)
lightIsTooBrightPad brightness = lightIsTooBrightPadBy (SubSyntSpec rndSaw mlp brightness)

lightIsTooBrightPadBy :: SubSynt -> Sig -> SE Sig
lightIsTooBrightPadBy spec = genDreamPadInstr spec mkOsc
    where 
        mkOsc vibLfo1 vibLfo2 x = uni wave (vibLfo1 x) + uni wave (vibLfo2 $ x * cent 14) + mul 0.3 (mul (uosc 0.25) (rndTri (vibLfo2 $ x * 7 * cent 4)) + mul (isawSeq [1, 0.5, 0.25] 6 * uosc 0.17) (rndTri (vibLfo2 $ x * 13)) + mul (sqrSeq [1, 0.5, 0.25, 0.1] 8 * uosc 0.28) (rndOsc (vibLfo2 $ x * 9 * cent 3)))
        wave = subOsc spec

whaleSongPadBy :: SubSynt -> Sig -> SE Sig
whaleSongPadBy spec = genDreamPadInstr spec mkOsc
    where 
        mkOsc vibLfo1 vibLfo2 x = uni wave (vibLfo1 x) + uni wave (vibLfo2 $ x * cent 14) + uni wave (vibLfo2 $ 3 * x * cent 14) + mul 0.15 (uni wave (vibLfo2 $ 7 * x * cent 14)) + mul 0.15 (uni wave ((vibLfo2 $ 11 * x * cent 14) + 400 * uosc 0.2))
        wave = subOsc spec

dreamPadBy :: SubSynt -> Sig -> SE Sig
dreamPadBy spec = genDreamPadInstr spec mkOsc
    where 
        mkOsc vibLfo1 vibLfo2 x = uni wave (vibLfo1 x) + uni wave (vibLfo2 $ x * cent 14) 
        wave = subOsc spec


genDreamPadInstr subSynt mkOsc x = do
    a1 <- oscs
    a2 <- nois
    return $ mul (fades 0.85 0.95) $ fx1 (a2 + a1) + fx2 a1   
    where
        brightness = subBright subSynt        
        filter = subFilter subSynt

        fx1 = filt 2 filter (filtLfo1 (700 + brightness * 2500)) 0.26
        fx2 = filter (filtLfo2 (1200 + brightness * 2500)) 0.32

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

impulseMarimba1 :: Sig -> Sig
impulseMarimba1 cps = mul 4 $ at (mlp cps 0.95) $ impulse 0

impulseMarimba2 :: Sig -> Sig
impulseMarimba2 cps = bat (bp  cps  120) $ impulse 0
