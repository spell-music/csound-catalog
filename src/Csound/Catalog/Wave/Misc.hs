module Csound.Catalog.Wave.Misc (
    okComputer        
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
