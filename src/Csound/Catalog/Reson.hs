-- | Resonators
module Csound.Catalog.Reson(
    Reson,
    -- * Vowels

    -- | Resonators for the vowel sounds.
    anO, anA, anE, anIY, anO2, wow     
) where

import Csound.Base

-- | List of pairs of 
--
-- > [(centerFrequency, bandWidth)]
--
-- It's a list of parameters for a bunch of the band pass filters (like reson, or bp).
-- Reson is intended to be used with functions 'Csound.Air.resons' and 'Csound.Air.resonsBy'.
type Reson = [(Sig, Sig)]

anO, anA, anE, anIY, anO2 :: Reson

anO  = [(280, 20), (650, 25), (2200, 30), (3450, 40), (4500, 50)]
anA  = [(650, 50), (1100, 50), (2860, 50), (3300, 50), (4500, 50)] 
anE  = [(500, 50), (1750, 50), (2450, 50), (3350, 50), (5000, 50)]
anIY = [(330, 50), (2000, 50), (2800, 50), (3650, 50), (5000, 50)]
anO2 = [(400, 50), (840, 50), (2800, 50), (3250, 50), (4500, 50)]

-- | Produces a 'wow'-effect if modifier rises and then falls down.
--
-- > y = wow modifierSig x
wow :: Sig -> Reson -> Reson
wow kmod = fmap $ \(a, b) -> (a + kmod, b)


