-- | Timbres
module Csound.Catalog.Wave(
    -- * Vowel
    --
    -- | An emulation of the singing of the vowels with granular synthesis (fof-opcode in the Csound)
    -- It's best to use these functions with vibrato.
    --
    -- > vibrato 0.12 5 $ oneVowel maleA 330
    vowels, loopVowels, oneVowel, Vowel,
    
    -- ** Vowels
    maleA, maleE, maleIY, maleO, maleOO, maleU, maleER, maleUH,
    femaleA, femaleE, femaleIY, femaleO, femaleOO,    
    
    -- * Woodwind instruments
    flute, bassClarinet, frenchHorn, sheng, hulusi, dizi,

    -- * Michael Gogins gallery
    pulseWidth,
    xanadu1, xanadu2, stringPad, toneWheel,
    guitar, harpsichord, xing,
    fmMod, filteredChorus, plainString, tubularBell, 
    delayedString, melody, rhodes, 
    
    -- * Bay at night
    -- | Instruments from the piece \"Bay at night\" by JL Diaz.
    nightPad,

    -- * Vestige of time
    -- | Instruments from the piece \"Vestige of time\" by Matthew Mariano.
    filteredSaw, filteredSawRising, filteredSawFalling,
    filteredNoise, 
    resonInstr, resonVibrato, 
    delaySaw, femaleVowel, amBell,

    -- * Desrted
    -- | Instruments from the piece \"Desrted\" by Jen Scaturro.
    simpleMarimba, marimbaWave, phasingSynth, noiz, wind,
    
    -- * The Heartbeat
    -- | Instruments from the piece \"The Heartbeat\" by Julie Friedman.
    crackle, heartbeat, monoPluck, chorusel,
    
    -- * Trapped in convert
    -- | Instruments from the piece \"Trapped in convert\" by Richard Boulanger
    ivory, blue, black        

) where

import Csound.Catalog.Wave.Ac
import Csound.Catalog.Wave.VestigeOfTime
import Csound.Catalog.Wave.Vowel
import Csound.Catalog.Wave.Woodwind
import Csound.Catalog.Wave.Deserted
import Csound.Catalog.Wave.TheHeartbeat
import Csound.Catalog.Wave.TrappedInConvert

import Csound.Base

import Csound.Catalog.Envelope(fadeIn)

-- | 
-- > nightPad fadeInTime cps
nightPad :: D -> Sig -> Sig
nightPad dt = gain (fadeIn dt) . stringPad 1

