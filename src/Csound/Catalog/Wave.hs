-- | Timbres
module Csound.Catalog.Wave(
    
    -- * Woodwind instruments
    flute, bassClarinet, frenchHorn, sheng, hulusi, dizi,

    -- * Michael Gogins gallery
    pulseWidth,
    xanadu1, xanadu2, stringPad, toneWheel,
    guitar, harpsichord, xing,
    fmMod, filteredChorus, plainString, fmTubularBell, 
    delayedString, melody, rhodes, 
    
    -- * Bay at night
    -- | Instruments from the piece \"Bay at night\" by JL Diaz.
    nightPad,

    -- * Vestige of time
    -- | Instruments from the piece \"Vestige of time\" by Matthew Mariano.
    filteredSaw, filteredSawRising, filteredSawFalling,
    filteredNoise, 
    resonInstr, simpleResonInstr, resonVibrato, 
    delaySaw, femaleVowel, amBell,

    -- * Desrted
    -- | Instruments from the piece \"Desrted\" by Jen Scaturro.
    simpleMarimba, marimbaWave, phasingSynth, noiz, wind,
    
    -- * The Heartbeat
    -- | Instruments from the piece \"The Heartbeat\" by Julie Friedman.
    heartbeat, monoPluck, chorusel,
    
    -- * Trapped in convert
    -- | Instruments from the piece \"Trapped in convert\" by Richard Boulanger
    ivory, blue, black, blackMarimba,
   
    -- * Modes
    -- | Percussive instruments defined with modal synthesis    -- (see the functions 'Csound.Air.modes' and 'Csound.Catalog.Reson.modesInstr'). All instruments take in a frequency and produce the output signal with percussive envelope.
    dahina, banyan, xylophone, tibetanBowl180, 
    spinelSphere, potLid, redCedarWoodPlate, 
    tubularBell, redwoodPlate, douglasFirWoodPlate,
    uniformWoodenBar, uniformAluminumBar, vibraphone1, 
    vibraphone2, chalandiPlates, tibetanBowl152, 
    tibetanBowl140, wineGlass, smallHandbell, 
    albertClockBellBelfast, woodBlock,

    -- * Vowel
    --
    -- | An emulation of the singing of the vowels with granular synthesis (fof-opcode in the Csound)
    -- It's best to use these functions with vibrato.
    --
    -- > vibrato 0.12 5 $ oneVowel maleA 330
    vowels, loopVowels, oneVowel, Vowel,
    
    -- ** Vowels
    maleA, maleE, maleIY, maleO, maleOO, maleU, maleER, maleUH,
    femaleA, femaleE, femaleIY, femaleO, femaleOO    

) where

import Csound.Base

import Csound.Catalog.Wave.Ac
import Csound.Catalog.Wave.VestigeOfTime
import Csound.Catalog.Wave.Vowel
import Csound.Catalog.Wave.Woodwind
import Csound.Catalog.Wave.Deserted
import Csound.Catalog.Wave.TheHeartbeat
import Csound.Catalog.Wave.TrappedInConvert

import Csound.Catalog.Reson

-- | 
-- > nightPad fadeInTime cps
nightPad :: D -> Sig -> Sig
nightPad dt = gain (fadeIn dt) . stringPad 1


-- modal synthesis
    
dahina, banyan, xylophone, tibetanBowl180, 
    spinelSphere, potLid, redCedarWoodPlate, 
    tubularBell, redwoodPlate, douglasFirWoodPlate,
    uniformWoodenBar, uniformAluminumBar, vibraphone1, 
    vibraphone2, chalandiPlates, tibetanBowl152, 
    tibetanBowl140, wineGlass, smallHandbell, 
    albertClockBellBelfast, woodBlock :: Sig -> Sig

dahina = strikeModes dahinaModes
banyan = strikeModes banyanModes 
xylophone = strikeModes xylophoneModes 
tibetanBowl180 = strikeModes tibetanBowlModes180  
spinelSphere = strikeModes spinelSphereModes 
potLid = strikeModes potLidModes 
redCedarWoodPlate = strikeModes redCedarWoodPlateModes 
tubularBell = strikeModes tubularBellModes 
redwoodPlate = strikeModes redwoodPlateModes 
douglasFirWoodPlate = strikeModes douglasFirWoodPlateModes 
uniformWoodenBar = strikeModes uniformWoodenBarModes 
uniformAluminumBar = strikeModes uniformAluminumBarModes 
vibraphone1 = strikeModes vibraphoneModes1 
vibraphone2 = strikeModes vibraphoneModes2 
chalandiPlates = strikeModes chalandiPlatesModes 
tibetanBowl152 = strikeModes tibetanBowlModes152 
tibetanBowl140 = strikeModes tibetanBowlModes140 
wineGlass = strikeModes wineGlassModes 
smallHandbell = strikeModes smallHandbellModes 
albertClockBellBelfast = strikeModes albertClockBellBelfastModes 
woodBlock = strikeModes woodBlockModes 


