module Csound.Catalog.Wave.Fm(
    fmBass1
) where

import Csound.Base

fmBass1 :: MonoAdsr -> (Sig, Sig) -> Sig
fmBass1 env (amp, cps) = mul 0.45 $ bhp 35 $ env 0.01 3 0.01 0.05 * (port amp 0.01) * (\x -> fosc 2 1 (1.5 * env 0.01 0.5 0.5 0.05) x + 0.4 * osc (x * 0.501)) (cps * (let env1 = linseg [0, 0.35, 0, 0.5, 1] * env 1.2 0.75 0.25 0.05 in 1 + (0.02 * env1 * uosc (3 * env1))))