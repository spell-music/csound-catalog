-- | Patches
--
-- Collection of beautiful timbres. To try the instrument with midi device just type in the interpreter:
--
-- > > dac $ atMidi hammondOrgan
-- If you don't have the real device, you can try the virtual midi:
--
-- > > vdac $ atMidi vibraphone1
--
-- The function @atMidi@ invokes a @Patch@ with midi.
module Csound.Patch(
	-- * Electric piano
	Epiano1(..), epiano1, epiano1', 
	MutedPiano(..), mutedPiano, mutedPiano',
	amPiano, fmPiano, 
	epiano2, epianoHeavy, epianoBright,
	vibraphonePiano1, vibraphonePiano2,
	addHammer,

	-- * Organ
	cathedralOrgan, toneWheelOrgan, 
	HammondOrgan(..), hammondOrgan, hammondOrgan',
	sawOrgan, triOrgan, sqrOrgan, pwOrgan, waveOrgan,

	hammondOrganm, hammondOrganm', sawOrganm, triOrganm, sqrOrganm, pwOrganm, waveOrganm,

	-- * Accordeon
	accordeon, accordeonBright1, accordeonBright2, brokenAccordeon,
	accordeon', Accordeon(..),

	-- * Choir
	choirA, choirO, choirU, choirE,
	Choir(..), choirA', choirO', choirU', choirE',

	windSings, noisyChoir, longNoisyChoir, noisyChoir', longNoisyChoir', NoisyChoir(..),

	-- * Pad
	pwPad, triPad, nightPad, overtonePad, caveOvertonePad,
	chorusel, pwEnsemble, fmDroneSlow, fmDroneMedium, fmDroneFast, vibrophonePad,
	RazorPad(..), razorPadSlow, razorPadFast, razorPadTremolo, razorPad, razorPad',
	dreamPad, underwaterPad, lightIsTooBrightPad, whaleSongPad, dreamPadBy,
	dreamPad', underwaterPad', lightIsTooBrightPad', whaleSongPad', dreamPad', dreamPadBy',

	-- ** Pad Monosynth
	pwPadm, triPadm, nightPadm, overtonePadm, caveOvertonePadm, choruselm,
	pwEnsemblem, fmDroneSlowm, fmDroneMediumm, fmDroneFastm, 
	razorPadSlowm, razorPadFastm, razorPadTremolom, razorPadm, razorPadm',
	dreamPadm, dreamPadBym, underwaterPadm, lightIsTooBrightPadm, whaleSongPadm, dreamPadm', underwaterPadm', dreamPadBym',
	lightIsTooBrightPadm', whaleSongPadm',

	-- * Lead
	polySynth, 
	phasingLead, RazorLead(..), razorLeadSlow, razorLeadFast, razorLeadTremolo,
	razorLead, razorLead',
	overtoneLead,

	-- ** Lead Monosynth
	polySynthm,

	-- * Bass
	simpleBass, pwBass, deepBass, withDeepBass,

	-- * Plucked
	guitar, harpsichord,	

	-- * Strike

	smallDahina, dahina, largeDahina, magicDahina,
	smallBanyan,banyan, largeBanyan, magicBanyan,
	smallXylophone, xylophone, largeXylophone, magicXylophone,
	smallTibetanBowl180, tibetanBowl180, largeTibetanBowl180, magicTibetanBowl180,
	smallSpinelSphere, spinelSphere, largeSpinelSphere, magicSpinelSphere, 
	smallPotLid, potLid, largePotLid, magicPotLid,
	smallRedCedarWoodPlate, redCedarWoodPlate, largeRedCedarWoodPlate, magicRedCedarWoodPlate,
	smallTubularBell, tubularBell, largeTubularBell, magicTubularBell,
	smallRedwoodPlate, redwoodPlate, largeRedwoodPlate, magicRedwoodPlate, smallDouglasFirWoodPlate,
	douglasFirWoodPlate, largeDouglasFirWoodPlate, magicDouglasFirWoodPlate, smallUniformWoodenBar,
	uniformWoodenBar, largeUniformWoodenBar, magicUniformWoodenBar, smallUniformAluminumBar,
	uniformAluminumBar, largeUniformAluminumBar, magicUniformAluminumBar, 
	smallVibraphone1, vibraphone1, largeVibraphone1, magicVibraphone1,
	smallVibraphone2, vibraphone2, largeVibraphone2, magicVibraphone2,
	smallChalandiPlates, chalandiPlates, largeChalandiPlates, magicChalandiPlates,
	smallTibetanBowl152, tibetanBowl152, largeTibetanBowl152, magicTibetanBowl152,
	smallTibetanBowl140, tibetanBowl140, largeTibetanBowl140, magicTibetanBowl140,
	smallWineGlass, wineGlass, largeWineGlass, magicWineGlass, 
	smallHandbell, handbell, largeHandbell, magicHandbell,
	smallAlbertClockBellBelfast, albertClockBellBelfast, largeAlbertClockBellBelfast, magicAlbertClockBellBelfast,
	smallWoodBlock, woodBlock, largeWoodBlock, magicWoodBlock,

	-- * Scrape
	scrapeDahina, scrapeBanyan, scrapeXylophone, scrapeTibetanBowl180, scrapeSpinelSphere, scrapePotLid, scrapeRedCedarWoodPlate,
	scrapeTubularBell, scrapeRedwoodPlate, scrapeDouglasFirWoodPlate, scrapeUniformWoodenBar, scrapeUniformAluminumBar,
	scrapeVibraphone1, scrapeVibraphone2, scrapeChalandiPlates, scrapeTibetanBowl152, scrapeTibetanBowl140, scrapeWineGlass,
	scrapeSmallHandbell, scrapeAlbertClockBellBelfast, scrapeWoodBlock, 

	scrapeFastDahina, scrapeFastBanyan, scrapeFastXylophone, scrapeFastTibetanBowl180, scrapeFastSpinelSphere, scrapeFastPotLid,
	scrapeFastRedCedarWoodPlate, scrapeFastTubularBell, scrapeFastRedwoodPlate, scrapeFastDouglasFirWoodPlate, scrapeFastUniformWoodenBar,
	scrapeFastUniformAluminumBar, scrapeFastVibraphone1, scrapeFastVibraphone2, scrapeFastChalandiPlates, scrapeFastTibetanBowl152,
	scrapeFastTibetanBowl140, scrapeFastWineGlass, scrapeFastSmallHandbell, scrapeFastAlbertClockBellBelfast, scrapeFastWoodBlock,

	scrapePadDahina, scrapePadBanyan, scrapePadXylophone, scrapePadTibetanBowl180, scrapePadSpinelSphere, scrapePadPotLid,
	scrapePadRedCedarWoodPlate, scrapePadTubularBell, scrapePadRedwoodPlate, scrapePadDouglasFirWoodPlate, scrapePadUniformWoodenBar,
	scrapePadUniformAluminumBar, scrapePadVibraphone1, scrapePadVibraphone2, scrapePadChalandiPlates, scrapePadTibetanBowl152,
	scrapePadTibetanBowl140, scrapePadWineGlass, scrapePadSmallHandbell, scrapePadAlbertClockBellBelfast, scrapePadWoodBlock,


	-- ** Scrape monosynth
	-- | Unfortunately they don't work with @atMonoMidi@. Though @atNote@ works fine. 
	scrapeDahinam, scrapeBanyanm, scrapeXylophonem, scrapeTibetanBowl180m, scrapeSpinelSpherem, scrapePotLidm, scrapeRedCedarWoodPlatem,
	scrapeTubularBellm, scrapeRedwoodPlatem, scrapeDouglasFirWoodPlatem, scrapeUniformWoodenBarm, scrapeUniformAluminumBarm,
	scrapeVibraphone1m, scrapeVibraphone2m, scrapeChalandiPlatesm, scrapeTibetanBowl152m, scrapeTibetanBowl140m, scrapeWineGlassm,
	scrapeSmallHandbellm, scrapeAlbertClockBellBelfastm, scrapeWoodBlockm, 

	scrapePadDahinam, scrapePadBanyanm, scrapePadXylophonem, scrapePadTibetanBowl180m, scrapePadSpinelSpherem, scrapePadPotLidm,
	scrapePadRedCedarWoodPlatem, scrapePadTubularBellm, scrapePadRedwoodPlatem, scrapePadDouglasFirWoodPlatem, scrapePadUniformWoodenBarm,
	scrapePadUniformAluminumBarm, scrapePadVibraphone1m, scrapePadVibraphone2m, scrapePadChalandiPlatesm, scrapePadTibetanBowl152m,
	scrapePadTibetanBowl140m, scrapePadWineGlassm, scrapePadSmallHandbellm, scrapePadAlbertClockBellBelfastm, scrapePadWoodBlockm,

	-- * Woodwind

	Wind(..), woodWind',

	fluteSpec, shortFluteSpec, 
	flute, shortFlute, fluteVibrato, mutedFlute, brightFlute, 

	bassClarinetSpec, shortBassClarinetSpec,
	bassClarinet, shortBassClarinet, bassClarinetVibrato, mutedBassClarinet, brightBassClarinet,

	frenchHornSpec, shortFrenchHornSpec, 
	frenchHorn, shortFrenchHorn, frenchHornVibrato, mutedFrenchHorn, brightFrenchHorn, 

	shengSpec, shortShengSpec,
	sheng, shortSheng, shengVibrato, mutedSheng, brightSheng,

	hulusiSpec, shortHulusiSpec, 
	hulusi, shortHulusi, hulusiVibrato, mutedHulusi, brightHulusi,

	diziSpec, shortDiziSpec, 
	dizi, shortDizi, diziVibrato, mutedDizi, brightDizi,

	-- * SHARC instruments
	SharcInstr,
	soloSharc, orcSharc, padSharc, purePadSharc, dreamSharc, dreamSharc',

	-- ** concrete instruments
	shViolin, shViolinPizzicato, shViolinMuted, shViolinMarteleBowing, shViolinsEnsemble, shViola, shViolaPizzicato, shViolaMuted,
    shViolaMarteleBowing, shTuba, shTromboneMuted, shTrombone, shPiccolo, shOboe, shFrenchHornMuted, shFrenchHorn, shFlute,
    shEnglishHorn, shClarinetEflat, shTrumpetMutedC, shTrumpetC, shContrabassClarinet, shContrabassoon, shCello, shCelloPizzicato,
    shCelloMuted, shCelloMarteleBowing, shContrabassPizzicato, shContrabassMuted, shContrabassMarteleBowing, shContrabass,
    shClarinet, shBassTrombone, shBassClarinet, shBassoon, shBassFlute, shTrumpetBach, shAltoTrombone, shAltoFlute,

	-- * X-rays
	pulseWidth, xanadu, alienIsAngry, noiz, blue, black, simpleMarimba, impulseMarimba1, impulseMarimba2, okComputer, noiseBell,

	-- * Robotic vowels
	robotVowels, robotLoopVowels, robotVowel,

	 -- ** Vowels
    maleA, maleE, maleIY, maleO, maleOO, maleU, maleER, maleUH,
    femaleA, femaleE, femaleIY, femaleO, femaleOO,

	-- * Nature
	windWall, mildWind, wind, snowCrackle,

	-- * Misc 
	limRel, singleFx, singleFx'
) where

import Control.Monad

import Csound.Base 

import qualified Csound.Catalog.Wave as C
import qualified Csound.Catalog.Reson as C

import Csound.Catalog.Wave(maleA, maleE, maleIY, maleO, maleOO, maleU, maleER, maleUH,
    femaleA, femaleE, femaleIY, femaleO, femaleOO)

import Csound.Catalog.Wave(Accordeon(..),
	SharcInstr,
	shViolin, shViolinPizzicato, shViolinMuted, shViolinMarteleBowing, shViolinsEnsemble, shViola, shViolaPizzicato, shViolaMuted,
    shViolaMarteleBowing, shTuba, shTromboneMuted, shTrombone, shPiccolo, shOboe, shFrenchHornMuted, shFrenchHorn, shFlute,
    shEnglishHorn, shClarinetEflat, shTrumpetMutedC, shTrumpetC, shContrabassClarinet, shContrabassoon, shCello, shCelloPizzicato,
    shCelloMuted, shCelloMarteleBowing, shContrabassPizzicato, shContrabassMuted, shContrabassMarteleBowing, shContrabass,
    shClarinet, shBassTrombone, shBassClarinet, shBassoon, shBassFlute, shTrumpetBach, shAltoTrombone, shAltoFlute)

import Data.Char

onSig1 :: SigSpace a => (Sig -> a) -> Sig2 -> a
onSig1 f (amp, cps) = mul amp $ f cps

fx1 :: Sig -> (a -> a) -> [FxSpec a]
fx1 dw f = [FxSpec dw (return . f)]

fx1' :: Sig -> (a -> SE a) -> [FxSpec a]
fx1' dw f = [FxSpec dw f]

-- | Creates a simple FX-xhain, that contains a single pure effect.
-- The first argument is the dry/wet-value.
singleFx :: Sig -> (a -> a) -> [FxSpec a]
singleFx = fx1

-- | Creates a simple FX-xhain, that contains a single effect.
-- The first argument is the dry/wet-value.
singleFx' :: Sig -> (a -> SE a) -> [FxSpec a]
singleFx' = fx1'

-- | Limits the release section of the note.
limRel :: SigSpace b => D -> Patch a b -> Patch a b
limRel rel p = p { patchInstr = fmap (mul (fadeOut rel)) . patchInstr p }

----------------------------------------------
-- electric pianos

data Epiano1 = Epiano1 
	{ epiano1Rel :: D }

instance Default Epiano1 where
	def = Epiano1 5	

epiano1 = epiano1' def

epiano1' (Epiano1 rel) = Patch 
	{ patchInstr = \a -> mul 0.4 $ C.simpleFading rel a
	, patchFx    = fx1 0.25 largeHall2 }

data MutedPiano = MutedPiano 
	{ mutedPianoMute :: Sig
	, mutedPianoRel  :: D }

instance Default MutedPiano where
	def = MutedPiano 0.5 7

mutedPiano = mutedPiano' def

mutedPiano' (MutedPiano mute rel) = Patch 
	{ patchInstr = \a -> mul 0.7 $ C.simpleSust rel a
	, patchFx    = fx1 0.25 (largeHall2 . at (mlp3 (250 + 7000 * mute) 0.2)) }

amPiano = Patch
	{ patchInstr = mul 1.4 . onCps C.amPiano
	, patchFx    = fx1 0.25 id }

fmPiano = Patch
	{ patchInstr = at fromMono . mul 0.75 . onCps (C.fmFlavio 6 3)
	, patchFx    = fx1 0.15 smallHall2 }

epiano2 = addHammer 0.15 $ Patch 
	{ patchInstr = mul 1.125 . at fromMono . (onCps $ C.epiano [C.EpianoOsc 4 5 1 1, C.EpianoOsc 8 10 2.01 1])
	, patchFx    = fx1 0.25 smallHall2 }

epianoHeavy = addHammer 0.15 $ Patch 
	{ patchInstr = mul 1.125 . at fromMono . (onCps $ C.epiano [C.EpianoOsc 4 5 1 1, C.EpianoOsc 8 10 2.01 1, C.EpianoOsc 8 15 0.5 0.5])
	, patchFx    = fx1 0.2 smallHall2 }

epianoBright = addHammer 0.15 $ Patch 
	{ patchInstr = mul 1.12 . at fromMono . (onCps $ C.epiano [C.EpianoOsc 4 5 1 1, C.EpianoOsc 8 10 3.01 1, C.EpianoOsc 8 15 5 0.5, C.EpianoOsc 8 4 7 0.3])
	, patchFx    = fx1 0.2 smallHall2 }

vibraphonePiano1 = addHammer 0.15 $ smallVibraphone1 { patchInstr = mul (1.5 * fadeOut 0.25) . at (mlp 6500 0.1). patchInstr smallVibraphone1 }
vibraphonePiano2 = addHammer 0.15 $ smallVibraphone2 { patchInstr = mul (1.5 * fadeOut 0.25) . at (mlp 6500 0.1). patchInstr smallVibraphone2 }

-- | Adds a hammer strike sound. The first argument is the amount of hammer sound.
addHammer :: Sig -> Patch2 -> Patch2
addHammer amt = mixInstr amt impulseMarimba2

----------------------------------------------
-- organs

cathedralOrgan = Patch
	{ patchInstr = at fromMono . mul 0.7 . onCps C.cathedralOrgan
	, patchFx    = fx1 0.27 largeHall2 }

-- [0, 30]
data HammondOrgan = HammondOrgan 
	{ hammondOrganDetune :: Sig }

instance Default HammondOrgan where
	def = HammondOrgan 12

hammondOrgan = hammondOrgan' def

hammondOrganm = hammondOrganm' def

hammondOrgan' (HammondOrgan detune) = Patch
	{ patchInstr = mul 0.4 . at fromMono . onCps (C.hammondOrgan detune)
	, patchFx    = fx1 0.15 smallRoom2 }

hammondOrganm' (HammondOrgan detune) = Patch
	{ patchInstr = mul 0.4 . at fromMono . onSig1 (C.hammondOrgan detune)
	, patchFx    = fx1 0.15 smallRoom2 }

toneWheelOrgan = Patch
	{ patchInstr = at fromMono  . mul (0.6 * fadeOut 0.05) . onCps C.toneWheel
	, patchFx    = fx1 0.3 smallHall2 }

sawOrgan  = mul 0.45 $ waveOrgan rndSaw
triOrgan  = mul 0.5  $ waveOrgan rndTri
sqrOrgan  = mul 0.45 $ waveOrgan rndSqr
pwOrgan k = mul 0.45 $ waveOrgan (rndPw k)

sawOrganm  = mul 0.45 $ waveOrganm rndSaw
triOrganm  = mul 0.5  $ waveOrganm rndTri
sqrOrganm  = mul 0.45 $ waveOrganm rndSqr
pwOrganm k = mul 0.45 $ waveOrganm (rndPw k)

waveOrgan :: (Sig -> SE Sig) -> Patch2 
waveOrgan wave = Patch 
	{ patchInstr = onCps $ at fromMono . mul (fades 0.01 0.01) . at (mlp 3500 0.1) . wave
	, patchFx    = fx1 0.25 smallHall2	
	}

waveOrganm :: (Sig -> SE Sig) -> PatchSig2 
waveOrganm wave = Patch 
	{ patchInstr = onSig1 $ at fromMono . mul (fades 0.01 0.01) . at (mlp 3500 0.1) . wave
	, patchFx    = fx1 0.25 smallHall2	
	}

----------------------------------------------
-- accordeons

accordeon = accordeon' def

accordeonBright1 = accordeon' (C.Accordeon 1 5 3 7)
accordeonBright2 = accordeon' (C.Accordeon 1 6 3 13)

accordeonHeavy = accordeon' (C.Accordeon 1 0.501 2 1.005)
brokenAccordeon = accordeon' (C.Accordeon 1 1.07 2.02 0.5)

accordeon' spec = Patch
	{ patchInstr = mul 0.63 . onCps (C.accordeon spec)
	, patchFx    = fx1' 0.25 C.accordeonFx }

----------------------------------------------
-- choir

data Choir = Choir { choirVibr :: Sig }

instance Default Choir where
	def  = Choir 7

tenor' filt (Choir vib) = Patch 
	{ patchInstr = at fromMono . mul 0.15 . onCps (C.tenorOsc filt vib)
	, patchFx    = fx1 0.25 smallHall2 }

soprano' filt (Choir vib) = Patch 
	{ patchInstr = at fromMono . mul 0.15 . onCps (C.sopranoOsc filt vib)
	, patchFx    = fx1 0.25 smallHall2 }

choir' filt vib = Patch
	{ patchInstr = \(amp, cps) -> do
			ref <- newRef (0 :: Sig2)
			when1 (sig cps <=* 220) $ writeRef ref =<< (patchInstr (tenor'   filt vib) (amp, cps))
			when1 (sig cps >*  220) $ writeRef ref =<< (patchInstr (soprano' filt vib) (amp, cps))
			readRef ref
	, patchFx   = fx1 0.25 smallHall2 	
	}

choirA = choirA' def
choirO = choirO' def
choirE = choirE' def
choirU = choirU' def

choirA' = choir' singA
choirO' = choir' singO
choirE' = choir' singE
choirU' = choir' singU

data NoisyChoir = NoisyChoir 
	{ noisyChoirFilterNum :: Int
	, noisyChoirBw        :: Sig
	}

instance Default NoisyChoir where
	def = NoisyChoir 2 25

windSings = longNoisyChoir' (NoisyChoir 1 15)

longNoisyChoir = longNoisyChoir' def
noisyChoir = noisyChoir' def


longNoisyChoir' (NoisyChoir n bw) = Patch
	{ patchInstr = at fromMono . mul 0.45 . onCps (C.noisyChoir n bw)
	, patchFx    = fx1 0.15 magicCave2 }

noisyChoir' ch = (longNoisyChoir' ch) { patchFx  = fx1 0.15 largeHall2 }

-- modes (wth delay or not delay)
--
--  dac $ mixAt 0.15 largeHall2 $ mixAt 0.2 (echo 0.25 0.45) $ at fromMono $ midi $ onMsg $ onCps (mul (fadeOut 2) . C.tibetanBowl152    )

----------------------------------------------
-- pads

pwPad = Patch
	{ patchInstr = mul 0.6 . at fromMono . onCps C.pwPad
	, patchFx    = fx1 0.25 smallHall2 }

pwPadm = Patch
	{ patchInstr = mul 0.6 . at fromMono . onSig1 C.pwPad
	, patchFx    = fx1 0.25 smallHall2 }

triPad = Patch 
	{ patchInstr = fmap fromMono . mul 0.7 . onCps C.triPad
	, patchFx  = fx1' 0.25 C.triPadFx }

triPadm = Patch 
	{ patchInstr = fmap fromMono . mul 0.7 . onSig1 C.triPad
	, patchFx  = fx1' 0.25 C.triPadFx }

nightPad = Patch
	{ patchInstr = mul 0.48 . at fromMono . onCps (mul (fadeOut 1) . C.nightPad 0.5)
	, patchFx    = fx1 0.25 largeHall2 }

nightPadm = Patch
	{ patchInstr = mul 0.48 . return . fromMono . onSig1 ((fadeOut 1 * ) . C.nightPad 0.5)
	, patchFx    = fx1 0.25 largeHall2 }

overtonePad = Patch
	{ patchInstr = mul 0.65 . at fromMono . mixAt 0.25 (mlp 1500 0.1) . onCps (\cps -> mul (fades 0.25 1.2) (C.tibetan 11 0.012 cps) + mul (fades 0.25 1) (C.tibetan 13 0.015 (cps * 0.5)))
	, patchFx    = fx1 0.35 smallHall2 }

overtonePadm = Patch
	{ patchInstr = mul 0.65 . return . fromMono . mixAt 0.25 (mlp 1500 0.1) . onSig1 (\cps -> mul (fades 0.25 1.2) (C.tibetan 11 0.012 cps) + mul (fades 0.25 1) (C.tibetan 13 0.015 (cps * 0.5)))
	, patchFx    = fx1 0.35 smallHall2 }

caveOvertonePad = overtonePad { patchFx = fx1 0.2 (magicCave2 . mul 0.8) }

caveOvertonePadm = overtonePadm { patchFx = fx1 0.2 (magicCave2 . mul 0.8) }

chorusel = Patch
	{ patchInstr = mul 0.9 . at (mlp (3500 + 2000 * uosc 0.1) 0.1) . onCps (mul (fades 0.65 1) . C.chorusel 13 0.5 10)
	, patchFx    = fx1 0.35 smallHall2 }

choruselm = Patch
	{ patchInstr = mul 0.9 . return . at (mlp (3500 + 2000 * uosc 0.1) 0.1) . onSig1 (mul (fades 0.65 1) . C.chorusel 13 0.5 10)
	, patchFx    = fx1 0.35 smallHall2 }

pwEnsemble = Patch
	{ patchInstr = at fromMono . mul 0.55 . onCps C.pwEnsemble
	, patchFx    = fx1 0.25 smallHall2 }

pwEnsemblem = Patch
	{ patchInstr = at fromMono . mul 0.55 . onSig1 C.pwEnsemble
	, patchFx    = fx1 0.25 smallHall2 }

fmDroneSlow = Patch
	{ patchInstr = at fromMono . mul 0.5 . onCps (C.fmDrone 3 (10, 5))
	, patchFx    = fx1 0.35 largeHall2 }

fmDroneSlowm = Patch
	{ patchInstr = return . at fromMono . mul 0.5 . onSig1 (C.fmDrone 3 (10, 5))
	, patchFx    = fx1 0.35 largeHall2 }

fmDroneMedium = Patch
	{ patchInstr = at fromMono . mul 0.5 . onCps (C.fmDrone 3 (5, 3))
	, patchFx    = fx1 0.25 smallHall2 }

fmDroneMediumm = Patch
	{ patchInstr = return . at fromMono . mul 0.5 . onSig1 (C.fmDrone 3 (5, 3))
	, patchFx    = fx1 0.25 smallHall2 }

fmDroneFast = Patch
	{ patchInstr = at fromMono . mul 0.5 . onCps (C.fmDrone 3 (0.5, 1))
	, patchFx    = fx1 0.25 smallHall2 } 

fmDroneFastm = Patch
	{ patchInstr = return . at fromMono . mul 0.5 . onSig1 (C.fmDrone 3 (0.5, 1))
	, patchFx    = fx1 0.25 smallHall2 } 

vibrophonePad = largeVibraphone1 { patchInstr = mul (1.5 * fades 0.5 0.25) . at (mlp 2500 0.1). patchInstr largeVibraphone1 }

data RazorPad = RazorPad { razorPadSpeed :: Sig }

instance Default RazorPad where
	def = RazorPad 0.5

razorPadSlow = razorPad' (def { razorPadSpeed = 0.1 })
razorPadFast = razorPad' (def { razorPadSpeed = 1.7 })
razorPadTremolo = razorPad' (def { razorPadSpeed = 6.7 })

razorPadSlowm = razorPadm' (def { razorPadSpeed = 0.1 })
razorPadFastm = razorPadm' (def { razorPadSpeed = 1.7 })
razorPadTremolom = razorPadm' (def { razorPadSpeed = 6.7 })

razorPad = razorPad' def

razorPadm = razorPadm' def

razorPad' (RazorPad speed) = Patch
	{ patchInstr = at fromMono . mul 0.6 . onCps (uncurry $ C.razorPad speed)
	, patchFx    = fx1 0.35 largeHall2 }


razorPadm' (RazorPad speed) = Patch
	{ patchInstr = at fromMono . mul 0.6 . (uncurry $ C.razorPad speed)
	, patchFx    = fx1 0.35 largeHall2 }

dreamPadFx = [FxSpec 0.35 (return . largeHall2), FxSpec 0.25 (at $ echo 0.25 0.65), FxSpec 0.25 (at $ chorus 0.07 1.25 1)]

dreamPad = dreamPad' 0.35
underwaterPad = underwaterPad' 0.35
lightIsTooBrightPad = lightIsTooBrightPad' 0.55
whaleSongPad = whaleSongPad' 0.35

dreamPadBy = dreamPadBy' 0.35

dreamPadm = dreamPadm' 0.35
underwaterPadm = underwaterPadm' 0.35
lightIsTooBrightPadm = lightIsTooBrightPadm' 0.55
whaleSongPadm = whaleSongPadm' 0.35

dreamPadBym = dreamPadBym' 0.35

-- | The first argument is brightness (0 to 1)
dreamPad' :: Sig -> Patch2
dreamPad' bright = Patch 
    { patchInstr = fmap fromMono . onCps (C.dreamPad bright)    
    , patchFx    = dreamPadFx
    }

-- | The first argument is brightness. The second argument is a wave shape function.
dreamPadBy' :: Sig -> (Sig -> SE Sig) -> Patch2
dreamPadBy' bright wave = Patch 
    { patchInstr = fmap fromMono . onCps (C.dreamPadBy wave bright)    
    , patchFx    = dreamPadFx
    }

-- | The first argument is brightness (0 to 1)
dreamPadm' :: Sig -> PatchSig2
dreamPadm' bright = Patch 
    { patchInstr = fmap fromMono . onSig1 (C.dreamPad bright)    
    , patchFx    = dreamPadFx
    }

-- | The first argument is brightness (0 to 1). The second argument is a wave function.
dreamPadBym' :: Sig -> (Sig -> SE Sig) -> PatchSig2
dreamPadBym' bright wave = Patch 
    { patchInstr = fmap fromMono . onSig1 (C.dreamPadBy wave bright)
    , patchFx    = dreamPadFx
    }

-- | The first argument is brightness (0 to 1)
underwaterPad' :: Sig -> Patch2
underwaterPad' bright = Patch 
    { patchInstr = fmap fromMono . onCps (C.underwaterPad bright)    
    , patchFx    = dreamPadFx
    }

-- | The first argument is brightness (0 to 1)
underwaterPadm' :: Sig -> PatchSig2
underwaterPadm' bright = Patch 
    { patchInstr = fmap fromMono . onSig1 (C.underwaterPad bright)    
    , patchFx    = dreamPadFx
    }

-- | The first argument is brightness (0 to 1)
lightIsTooBrightPad' :: Sig -> Patch2
lightIsTooBrightPad' bright = Patch 
    { patchInstr = fmap fromMono . onCps (C.lightIsTooBrightPad bright)    
    , patchFx    = dreamPadFx
    }

lightIsTooBrightPadm' :: Sig -> PatchSig2
lightIsTooBrightPadm' bright = Patch 
    { patchInstr = fmap fromMono . onSig1 (C.lightIsTooBrightPad bright)    
    , patchFx    = dreamPadFx
    }

-- | The first argument is brightness (0 to 1)
whaleSongPad' :: Sig -> Patch2
whaleSongPad' bright = Patch 
    { patchInstr = fmap fromMono . onCps (C.whaleSongPad bright)    
    , patchFx    = dreamPadFx
    }

whaleSongPadm' :: Sig -> PatchSig2
whaleSongPadm' bright = Patch 
    { patchInstr = fmap fromMono . onSig1 (C.whaleSongPad bright)    
    , patchFx    = dreamPadFx
    }

------------------------------------
-- leads

polySynth = Patch 
	{ patchInstr = fmap fromMono . onCps C.polySynth 	
	, patchFx    = [FxSpec 0.25 (return . largeHall2), FxSpec 0.25 (at $ echo 0.25 0.65), FxSpec 0.25 (at $ chorus 0.07 1.25 1)]
	}

polySynthm = Patch 
	{ patchInstr = fmap fromMono . onSig1 C.polySynth 	
	, patchFx    = [FxSpec 0.25 (return . largeHall2), FxSpec 0.25 (at $ echo 0.25 0.65), FxSpec 0.25 (at $ chorus 0.07 1.25 1)]
	}

phasingLead = Patch
	{ patchInstr = at fromMono . mul (0.7 * fadeOut 0.05) . onCps (uncurry C.phasingSynth)
	, patchFx    = fx1 0.25 smallHall2 }

data RazorLead = RazorLead 
	{ razorLeadBright :: Sig 
	, razorLeadSpeed  :: Sig }

instance Default RazorLead where
	def = RazorLead 0.5 0.5

razorLeadSlow = razorLead' (def { razorLeadSpeed = 0.1 })
razorLeadFast = razorLead' (def { razorLeadSpeed = 1.7 })
razorLeadTremolo = razorLead' (def { razorLeadSpeed = 6.7 })

razorLead = razorLead' def

razorLead' (RazorLead bright speed) = Patch
	{ patchInstr = at fromMono . (\(amp, cps) -> mul (fadeOut (0.05 + amp * 0.3)) $ C.razorLead (bright * sig amp) (speed * sig amp) (sig amp) (sig cps))
	, patchFx    = fx1 0.35 smallHall2 }

overtoneLeadFx :: Sig2 -> SE Sig2
overtoneLeadFx x = fmap magicCave2 $ mixAt 0.2 (echo 0.25 0.45) (return x)

overtoneLead :: Patch2
overtoneLead = Patch
	{ patchInstr = mul 0.4 . at fromMono . onCps (mul (fades 0.01 1) . C.tibetan 13 0.012)
	, patchFx    = fx1' 0.15 overtoneLeadFx }

------------------------------------
-- bass

simpleBass = Patch 
	{ patchInstr = at fromMono . mul 0.32 . onCps C.simpleBass
	, patchFx    = fx1 0.25 smallRoom2 }

pwBass = Patch 
	{ patchInstr = at fromMono . mul 0.4 . onCps C.pwBass
	, patchFx    = fx1 0.25 smallHall2 }

deepBass = Patch
	{ patchInstr = at fromMono . mul 0.4 . onCps C.deepBass
	, patchFx    = fx1 0.25 smallHall2 }

-- | The first argument is the amount of deepBass to mix into the original patch.
withDeepBass :: Sig -> Patch2 -> Patch2
withDeepBass k = mixInstr k deepBass

------------------------------------
-- plucked

guitar = Patch
	{ patchInstr = onCps $ fromMono . mul (0.6 * fades 0.01 0.25) . C.plainString
	, patchFx    = fx1 0.25 smallHall2 }

harpsichord = Patch
	{ patchInstr = onCps $ fromMono . mul (0.65 * fades 0.01 0.13) . C.harpsichord
	, patchFx    = fx1 0.25 smallHall2 }

-- guita

------------------------------------
-- strike

strikeFx :: Strike -> Sig2 -> SE Sig2
strikeFx spec a = at (strikeReverb spec) $ (if (strikeHasDelay spec) then (mixAt 0.35 (echo 0.25 0.55)) else id) (return a :: SE Sig2)

strikeRelease :: (D, D) -> Strike -> D
strikeRelease (amp, cps) spec = (0.85 * strikeRel spec * amp) * amp + (strikeRel spec) - (cps / 10000)

-- dac $ mixAt 0.15 largeHall2 $ mixAt 0.2 (echo 0.25 0.45) $ at fromMono $ midi $ onMsg $ onCps (mul (fadeOut 2) . C.tibetanBowl152 )
data Strike = Strike 
	{ strikeRel :: D
	, strikeHasDelay ::	Bool
	, strikeReverb :: Sig2 -> Sig2		
	}

instance Default Strike where
	def = Strike 1.5 True smallHall2

strike' :: Strike -> (Sig -> Sig) -> Patch2
strike' spec instr = Patch 
	{ patchInstr =  \x@(amp, cps) -> return $ fromMono $ mul (0.75 * sig amp * fadeOut (rel x)) $ instr (sig cps)
	, patchFx    = fx1' 0.25 (strikeFx spec) }
	where rel a = strikeRelease a spec


data Size = Small | Medium | Large | Huge

nextSize x = case x of
	Small -> Medium
	Medium -> Large
	Large -> Huge
	Huge -> Huge

prevSize x = case x of
	Small -> Small
	Medium -> Small
	Large -> Medium
	Huge -> Large

toStrikeSpec :: Size -> Size -> Strike
toStrikeSpec revSpec restSpec = Strike 
	{ strikeReverb  = toReverb revSpec
	, strikeRel = toRel restSpec
	, strikeHasDelay = toHasDelay restSpec } 

toReverb :: Size -> (Sig2  -> Sig2)
toReverb x = case x of
	Small -> smallRoom2
	Medium -> smallHall2
	Large -> largeHall2
	Huge -> magicCave2

toRel :: Size -> D
toRel x = case x of
	Small -> 0.4
	Medium -> 1.5
	Large -> 2.5
	Huge -> 4.5

toGain :: Size -> Sig
toGain x = case x of
	Small -> 0.85
	Medium -> 0.75
	Large -> 0.6
	Huge -> 0.45

toHasDelay :: Size -> Bool
toHasDelay x = case x of
	Small -> False
	_     -> True

dahinaSize    		= Small
banyanSize    		= Medium
xylophoneSize 		= Small
tibetanBowl152Size  	= Medium
tibetanBowl140Size  	= Small
tibetanBowl180Size  	= Medium
spinelSphereSize    	= Small
potLidSize          	= Medium
redCedarWoodPlateSize = Small
tubularBellSize     	= Large
redwoodPlateSize    	= Small
douglasFirWoodPlateSize = Small
uniformWoodenBarSize = Small
uniformAluminumBarSize = Small
vibraphone1Size = Medium
vibraphone2Size = Medium
chalandiPlatesSize = Medium
wineGlassSize  = Medium
smallHandbellSize = Medium
albertClockBellBelfastSize = Large
woodBlockSize = Small

smallStrike :: Size -> (Sig -> Sig) -> Patch2
smallStrike size = mediumStrike' (prevSize size) size

mediumStrike :: Size -> (Sig -> Sig) -> Patch2
mediumStrike size = mediumStrike' size size

largeStrike :: Size -> (Sig -> Sig) -> Patch2
largeStrike size = mediumStrike' (nextSize size) size

magicStrike :: Size -> (Sig -> Sig) -> Patch2
magicStrike size = mediumStrike' (nextSize $ nextSize size) size

mediumStrike' :: Size -> Size -> (Sig -> Sig) -> Patch2
mediumStrike' revSize size f = p { patchInstr = mul (toGain size) . patchInstr p }
	where p = strike' (toStrikeSpec revSize size) f


smallDahina = smallStrike dahinaSize C.dahina
dahina = mediumStrike dahinaSize C.dahina
largeDahina = largeStrike dahinaSize C.dahina
magicDahina = magicStrike dahinaSize C.dahina

smallBanyan = smallStrike banyanSize C.banyan
banyan = mediumStrike banyanSize C.banyan
largeBanyan = largeStrike banyanSize C.banyan
magicBanyan = magicStrike banyanSize C.banyan

smallXylophone = smallStrike xylophoneSize C.xylophone
xylophone = mediumStrike xylophoneSize C.xylophone
largeXylophone = largeStrike xylophoneSize C.xylophone
magicXylophone = magicStrike xylophoneSize C.xylophone

smallTibetanBowl180 = smallStrike tibetanBowl180Size C.tibetanBowl180
tibetanBowl180 = mediumStrike tibetanBowl180Size C.tibetanBowl180
largeTibetanBowl180 = largeStrike tibetanBowl180Size C.tibetanBowl180
magicTibetanBowl180 = magicStrike tibetanBowl180Size C.tibetanBowl180

smallSpinelSphere = smallStrike spinelSphereSize C.spinelSphere
spinelSphere = mediumStrike spinelSphereSize C.spinelSphere
largeSpinelSphere = largeStrike spinelSphereSize C.spinelSphere
magicSpinelSphere = magicStrike spinelSphereSize C.spinelSphere

smallPotLid = smallStrike potLidSize C.potLid
potLid = mediumStrike potLidSize C.potLid
largePotLid = largeStrike potLidSize C.potLid
magicPotLid = magicStrike potLidSize C.potLid

smallRedCedarWoodPlate = smallStrike redCedarWoodPlateSize C.redCedarWoodPlate
redCedarWoodPlate = mediumStrike redCedarWoodPlateSize C.redCedarWoodPlate
largeRedCedarWoodPlate = largeStrike redCedarWoodPlateSize C.redCedarWoodPlate
magicRedCedarWoodPlate = magicStrike redCedarWoodPlateSize C.redCedarWoodPlate

smallTubularBell = smallStrike tubularBellSize C.tubularBell
tubularBell = mediumStrike tubularBellSize C.tubularBell
largeTubularBell = largeStrike tubularBellSize C.tubularBell
magicTubularBell = magicStrike tubularBellSize C.tubularBell

smallRedwoodPlate = smallStrike redwoodPlateSize C.redwoodPlate
redwoodPlate = mediumStrike redwoodPlateSize C.redwoodPlate
largeRedwoodPlate = largeStrike redwoodPlateSize C.redwoodPlate
magicRedwoodPlate = magicStrike redwoodPlateSize C.redwoodPlate

smallDouglasFirWoodPlate = smallStrike douglasFirWoodPlateSize C.douglasFirWoodPlate
douglasFirWoodPlate = mediumStrike douglasFirWoodPlateSize C.douglasFirWoodPlate
largeDouglasFirWoodPlate = largeStrike douglasFirWoodPlateSize C.douglasFirWoodPlate
magicDouglasFirWoodPlate = magicStrike douglasFirWoodPlateSize C.douglasFirWoodPlate

smallUniformWoodenBar = smallStrike uniformWoodenBarSize C.uniformWoodenBar
uniformWoodenBar = mediumStrike uniformWoodenBarSize C.uniformWoodenBar
largeUniformWoodenBar = largeStrike uniformWoodenBarSize C.uniformWoodenBar
magicUniformWoodenBar = magicStrike uniformWoodenBarSize C.uniformWoodenBar

smallUniformAluminumBar = smallStrike uniformAluminumBarSize C.uniformAluminumBar
uniformAluminumBar = mediumStrike uniformAluminumBarSize C.uniformAluminumBar
largeUniformAluminumBar = largeStrike uniformAluminumBarSize C.uniformAluminumBar
magicUniformAluminumBar = magicStrike uniformAluminumBarSize C.uniformAluminumBar

smallVibraphone1 = smallStrike vibraphone1Size C.vibraphone1
vibraphone1 = mediumStrike vibraphone1Size C.vibraphone1
largeVibraphone1 = largeStrike vibraphone1Size C.vibraphone1
magicVibraphone1 = magicStrike vibraphone1Size C.vibraphone1

smallVibraphone2 = smallStrike vibraphone2Size C.vibraphone2
vibraphone2 = mediumStrike vibraphone2Size C.vibraphone2
largeVibraphone2 = largeStrike vibraphone2Size C.vibraphone2
magicVibraphone2 = magicStrike vibraphone2Size C.vibraphone2

smallChalandiPlates = smallStrike chalandiPlatesSize C.chalandiPlates
chalandiPlates = mediumStrike chalandiPlatesSize C.chalandiPlates
largeChalandiPlates = largeStrike chalandiPlatesSize C.chalandiPlates
magicChalandiPlates = magicStrike chalandiPlatesSize C.chalandiPlates

smallTibetanBowl152 = smallStrike tibetanBowl152Size C.tibetanBowl152
tibetanBowl152 = mediumStrike tibetanBowl152Size C.tibetanBowl152
largeTibetanBowl152 = largeStrike tibetanBowl152Size C.tibetanBowl152
magicTibetanBowl152 = magicStrike tibetanBowl152Size C.tibetanBowl152

smallTibetanBowl140 = smallStrike tibetanBowl140Size C.tibetanBowl140
tibetanBowl140 = mediumStrike tibetanBowl140Size C.tibetanBowl140
largeTibetanBowl140 = largeStrike tibetanBowl140Size C.tibetanBowl140
magicTibetanBowl140 = magicStrike tibetanBowl140Size C.tibetanBowl140

smallWineGlass = smallStrike wineGlassSize C.wineGlass
wineGlass = mediumStrike wineGlassSize C.wineGlass
largeWineGlass = largeStrike wineGlassSize C.wineGlass
magicWineGlass = magicStrike wineGlassSize C.wineGlass

smallHandbell = smallStrike smallHandbellSize C.smallHandbell
handbell = mediumStrike smallHandbellSize C.smallHandbell
largeHandbell = largeStrike smallHandbellSize C.smallHandbell
magicHandbell = magicStrike smallHandbellSize C.smallHandbell

smallAlbertClockBellBelfast = smallStrike albertClockBellBelfastSize C.albertClockBellBelfast
albertClockBellBelfast = mediumStrike albertClockBellBelfastSize C.albertClockBellBelfast
largeAlbertClockBellBelfast = largeStrike albertClockBellBelfastSize C.albertClockBellBelfast
magicAlbertClockBellBelfast = magicStrike albertClockBellBelfastSize C.albertClockBellBelfast

smallWoodBlock = smallStrike woodBlockSize C.woodBlock
woodBlock = mediumStrike woodBlockSize C.woodBlock
largeWoodBlock = largeStrike woodBlockSize C.woodBlock
magicWoodBlock = magicStrike woodBlockSize C.woodBlock

---------------------------------------------------------------
-- scrape

-- scrapePatch 

names = ["dahina","banyan","xylophone","tibetanBowl180","spinelSphere","potLid","redCedarWoodPlate","tubularBell","redwoodPlate","douglasFirWoodPlate","uniformWoodenBar","uniformAluminumBar","vibraphone1","vibraphone2","chalandiPlates","tibetanBowl152","tibetanBowl140","wineGlass","smallHandbell","albertClockBellBelfast","woodBlock"]
toUpperName (x:xs) = toUpper x : xs

-- scrapePatch 

scrapeRelease :: (D, D) -> D -> D
scrapeRelease (amp, cps) rel = (0.85 * rel * amp) * amp + rel - (cps / 10000)

scrapeFast k m = Patch 
	{ patchInstr = \x@(amp, cps) -> (mul (0.75 * sig amp * k * fades 0.02 (scrapeRelease x 0.25)) . at fromMono . C.scrapeModes m) (sig cps)
	, patchFx    = fx1 0.15 largeHall2 }

scrape k m = Patch 
	{ patchInstr = \x@(amp, cps) -> (mul (0.75 * sig amp * k * fades 0.5 (scrapeRelease x 0.97)) . at fromMono . C.scrapeModes m) (sig cps)
	, patchFx    = fx1 0.15 largeHall2 }

scrapem k m = Patch 
	{ patchInstr = \(amp, cps) -> (mul (0.75 * amp * k * fades 0.5 1.97) . at fromMono . C.scrapeModes m) cps
	, patchFx    = fx1 0.15 largeHall2 }

scrapePad k m = Patch 
	{ patchInstr = \x@(amp, cps) -> (mul (0.75 * sig amp * k * fades 0.5 (scrapeRelease x 2.27	)) . at fromMono . C.scrapeModes m) (sig cps)
	, patchFx    = fx1 0.15 largeHall2 }

scrapePadm k m = Patch 
	{ patchInstr = \(amp, cps) -> (mul (0.75 * amp * k * fades 0.5 2.27) . at fromMono . C.scrapeModes m) cps
	, patchFx    = fx1 0.15 largeHall2 }	

scaleScrapeDahina = 1.32
scaleScrapeBanyan = 0.95
scaleScrapeXylophone = 1
scaleScrapeTibetanBowl180 = 0.55
scaleScrapeSpinelSphere = 1.4
scaleScrapePotLid = 0.65
scaleScrapeRedCedarWoodPlate = 1
scaleScrapeTubularBell = 0.75
scaleScrapeRedwoodPlate = 1
scaleScrapeDouglasFirWoodPlate = 1
scaleScrapeUniformWoodenBar = 1
scaleScrapeUniformAluminumBar = 0.75
scaleScrapeVibraphone1 = 0.9
scaleScrapeVibraphone2 = 0.9
scaleScrapeChalandiPlates = 1
scaleScrapeTibetanBowl152 = 0.65
scaleScrapeTibetanBowl140 = 0.75
scaleScrapeWineGlass = 0.6
scaleScrapeSmallHandbell = 1
scaleScrapeAlbertClockBellBelfast = 0.5
scaleScrapeWoodBlock = 1.32

scrapeDahina = scrape scaleScrapeDahina C.dahinaModes
scrapeBanyan = scrape scaleScrapeBanyan C.banyanModes
scrapeXylophone = scrape scaleScrapeXylophone C.xylophoneModes
scrapeTibetanBowl180 = scrape scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapeSpinelSphere = scrape scaleScrapeSpinelSphere C.spinelSphereModes
scrapePotLid = scrape scaleScrapePotLid C.potLidModes
scrapeRedCedarWoodPlate = scrape scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapeTubularBell = scrape scaleScrapeTubularBell C.tubularBellModes
scrapeRedwoodPlate = scrape scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapeDouglasFirWoodPlate = scrape scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapeUniformWoodenBar = scrape scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapeUniformAluminumBar = scrape scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapeVibraphone1 = scrape scaleScrapeVibraphone1 C.vibraphoneModes1
scrapeVibraphone2 = scrape scaleScrapeVibraphone2 C.vibraphoneModes2
scrapeChalandiPlates = scrape scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapeTibetanBowl152 = scrape scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapeTibetanBowl140 = scrape scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapeWineGlass = scrape scaleScrapeWineGlass C.wineGlassModes
scrapeSmallHandbell = scrape scaleScrapeSmallHandbell C.smallHandbellModes
scrapeAlbertClockBellBelfast = scrape scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapeWoodBlock = scrape scaleScrapeWoodBlock C.woodBlockModes

scrapeDahinam = scrapem scaleScrapeDahina C.dahinaModes
scrapeBanyanm = scrapem scaleScrapeBanyan C.banyanModes
scrapeXylophonem = scrapem scaleScrapeXylophone C.xylophoneModes
scrapeTibetanBowl180m = scrapem scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapeSpinelSpherem = scrapem scaleScrapeSpinelSphere C.spinelSphereModes
scrapePotLidm = scrape scaleScrapePotLid C.potLidModes
scrapeRedCedarWoodPlatem = scrapem scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapeTubularBellm = scrapem scaleScrapeTubularBell C.tubularBellModes
scrapeRedwoodPlatem = scrapem scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapeDouglasFirWoodPlatem = scrapem scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapeUniformWoodenBarm = scrapem scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapeUniformAluminumBarm = scrapem scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapeVibraphone1m = scrapem scaleScrapeVibraphone1 C.vibraphoneModes1
scrapeVibraphone2m = scrapem scaleScrapeVibraphone2 C.vibraphoneModes2
scrapeChalandiPlatesm = scrapem scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapeTibetanBowl152m = scrapem scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapeTibetanBowl140m = scrapem scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapeWineGlassm = scrapem scaleScrapeWineGlass C.wineGlassModes
scrapeSmallHandbellm = scrapem scaleScrapeSmallHandbell C.smallHandbellModes
scrapeAlbertClockBellBelfastm = scrapem scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapeWoodBlockm = scrapem scaleScrapeWoodBlock C.woodBlockModes

scrapeFastDahina = scrapeFast scaleScrapeDahina C.dahinaModes
scrapeFastBanyan = scrapeFast scaleScrapeBanyan C.banyanModes
scrapeFastXylophone = scrapeFast scaleScrapeXylophone C.xylophoneModes
scrapeFastTibetanBowl180 = scrapeFast scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapeFastSpinelSphere = scrapeFast scaleScrapeSpinelSphere C.spinelSphereModes
scrapeFastPotLid = scrapeFast scaleScrapePotLid C.potLidModes
scrapeFastRedCedarWoodPlate = scrapeFast scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapeFastTubularBell = scrapeFast scaleScrapeTubularBell C.tubularBellModes
scrapeFastRedwoodPlate = scrapeFast scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapeFastDouglasFirWoodPlate = scrapeFast scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapeFastUniformWoodenBar = scrapeFast scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapeFastUniformAluminumBar = scrapeFast scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapeFastVibraphone1 = scrapeFast scaleScrapeVibraphone1 C.vibraphoneModes1
scrapeFastVibraphone2 = scrapeFast scaleScrapeVibraphone2 C.vibraphoneModes2
scrapeFastChalandiPlates = scrapeFast scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapeFastTibetanBowl152 = scrapeFast scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapeFastTibetanBowl140 = scrapeFast scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapeFastWineGlass = scrapeFast scaleScrapeWineGlass C.wineGlassModes
scrapeFastSmallHandbell = scrapeFast scaleScrapeSmallHandbell C.smallHandbellModes
scrapeFastAlbertClockBellBelfast = scrapeFast scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapeFastWoodBlock = scrapeFast scaleScrapeWoodBlock C.woodBlockModes

scrapePadDahina = scrapePad scaleScrapeDahina C.dahinaModes
scrapePadBanyan = scrapePad scaleScrapeBanyan C.banyanModes
scrapePadXylophone = scrapePad scaleScrapeXylophone C.xylophoneModes
scrapePadTibetanBowl180 = scrapePad scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapePadSpinelSphere = scrapePad scaleScrapeSpinelSphere C.spinelSphereModes
scrapePadPotLid = scrapePad scaleScrapePotLid C.potLidModes
scrapePadRedCedarWoodPlate = scrapePad scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapePadTubularBell = scrapePad scaleScrapeTubularBell C.tubularBellModes
scrapePadRedwoodPlate = scrapePad scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapePadDouglasFirWoodPlate = scrapePad scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapePadUniformWoodenBar = scrapePad scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapePadUniformAluminumBar = scrapePad scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapePadVibraphone1 = scrapePad scaleScrapeVibraphone1 C.vibraphoneModes1
scrapePadVibraphone2 = scrapePad scaleScrapeVibraphone2 C.vibraphoneModes2
scrapePadChalandiPlates = scrapePad scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapePadTibetanBowl152 = scrapePad scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapePadTibetanBowl140 = scrapePad scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapePadWineGlass = scrapePad scaleScrapeWineGlass C.wineGlassModes
scrapePadSmallHandbell = scrapePad scaleScrapeSmallHandbell C.smallHandbellModes
scrapePadAlbertClockBellBelfast = scrapePad scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapePadWoodBlock = scrapePad scaleScrapeWoodBlock C.woodBlockModes

scrapePadDahinam = scrapePadm scaleScrapeDahina C.dahinaModes
scrapePadBanyanm = scrapePadm scaleScrapeBanyan C.banyanModes
scrapePadXylophonem = scrapePadm scaleScrapeXylophone C.xylophoneModes
scrapePadTibetanBowl180m = scrapePadm scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapePadSpinelSpherem = scrapePadm scaleScrapeSpinelSphere C.spinelSphereModes
scrapePadPotLidm = scrapePadm scaleScrapePotLid C.potLidModes
scrapePadRedCedarWoodPlatem = scrapePadm scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapePadTubularBellm = scrapePadm scaleScrapeTubularBell C.tubularBellModes
scrapePadRedwoodPlatem = scrapePadm scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapePadDouglasFirWoodPlatem = scrapePadm scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapePadUniformWoodenBarm = scrapePadm scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapePadUniformAluminumBarm = scrapePadm scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapePadVibraphone1m = scrapePadm scaleScrapeVibraphone1 C.vibraphoneModes1
scrapePadVibraphone2m = scrapePadm scaleScrapeVibraphone2 C.vibraphoneModes2
scrapePadChalandiPlatesm = scrapePadm scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapePadTibetanBowl152m = scrapePadm scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapePadTibetanBowl140m = scrapePadm scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapePadWineGlassm = scrapePadm scaleScrapeWineGlass C.wineGlassModes
scrapePadSmallHandbellm = scrapePadm scaleScrapeSmallHandbell C.smallHandbellModes
scrapePadAlbertClockBellBelfastm = scrapePadm scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapePadWoodBlockm = scrapePadm scaleScrapeWoodBlock C.woodBlockModes


------------------------------------
-- woodwind

data Wind = Wind 
	{ windAtt :: D
	, windDec :: D
	, windSus :: D	
	, windVib :: D	
	, windBright :: D }

woodWind' spec instr = Patch 
	{ patchInstr = \(amp, cps) -> mul 1.3 $ do 
		seed <- rnd 1
		vibDisp <- rnd (0.1 * amp)
		let dispVib vib = vib * (0.9 + vibDisp)		
		return $ fromMono $ mul (0.8 * sig amp * fadeOut (windDec spec)) $ instr seed (dispVib $ windVib spec) (windAtt spec) (windSus spec) (windDec spec) (0.4 + 0.75 * windBright spec * amp) cps
	, patchFx    = fx1 0.25 smallHall2 }

-- flute

fluteSpec bright vib = Wind 
	{ windAtt = 0.08
	, windDec = 0.1
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortFluteSpec bright vib = Wind 
	{ windAtt = 0.03
	, windDec = 0.05
	, windSus = 20
	, windVib = vib
	, windBright = bright }

flute = woodWind' (fluteSpec br vib) C.flute
	where 
		br = 0.7
		vib = 0.015

shortFlute = woodWind' (shortFluteSpec br vib) C.flute
	where 
		br = 0.7
		vib = 0.015

fluteVibrato = woodWind' (fluteSpec br vib) C.flute
	where 
		br = 0.7
		vib = 0.04

mutedFlute = woodWind' (fluteSpec br vib) C.flute
	where 
		br = 0.25
		vib = 0.015

brightFlute = woodWind' (fluteSpec br vib) C.flute
	where 
		br = 1.2
		vib = 0.015

-- bass clarinet

bassClarinetSpec bright vib = Wind 
	{ windAtt = 0.06
	, windDec = 0.15
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortBassClarinetSpec bright vib = Wind 
	{ windAtt = 0.03
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

bassClarinet = woodWind' (bassClarinetSpec br vib) C.bassClarinet
	where 
		br = 0.7
		vib = 0.01

shortBassClarinet = woodWind' (shortBassClarinetSpec br vib) C.bassClarinet
	where 
		br = 0.7
		vib = 0.01

bassClarinetVibrato = woodWind' (bassClarinetSpec br vib) C.bassClarinet
	where 
		br = 0.7
		vib = 0.035

mutedBassClarinet = woodWind' (bassClarinetSpec br vib) C.bassClarinet
	where 
		br = 0.25
		vib = 0.01

brightBassClarinet = woodWind' (bassClarinetSpec br vib) C.bassClarinet
	where 
		br = 1.2
		vib = 0.01

-- french horn

frenchHornSpec bright vib = Wind 
	{ windAtt = 0.08
	, windDec = 0.25
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortFrenchHornSpec bright vib = Wind 
	{ windAtt = 0.03
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

frenchHorn = woodWind' (frenchHornSpec br vib) C.frenchHorn
	where 
		br = 0.7
		vib = 0.01

shortFrenchHorn = woodWind' (shortFrenchHornSpec br vib) C.frenchHorn
	where 
		br = 0.7
		vib = 0.01

frenchHornVibrato = woodWind' (frenchHornSpec br vib) C.frenchHorn
	where 
		br = 0.7
		vib = 0.035

mutedFrenchHorn = woodWind' (frenchHornSpec br vib) C.frenchHorn
	where 
		br = 0.25
		vib = 0.01

brightFrenchHorn = woodWind' (frenchHornSpec br vib) C.frenchHorn
	where 
		br = 1.2
		vib = 0.01

-- sheng

shengSpec bright vib = Wind 
	{ windAtt = 0.1
	, windDec = 0.2
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortShengSpec bright vib = Wind 
	{ windAtt = 0.03
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

sheng = woodWind' (shengSpec br vib) C.sheng
	where 
		br = 0.7
		vib = 0.01

shortSheng = woodWind' (shortShengSpec br vib) C.sheng
	where 
		br = 0.7
		vib = 0.01

shengVibrato = woodWind' (shengSpec br vib) C.sheng
	where 
		br = 0.7
		vib = 0.025

mutedSheng = woodWind' (shengSpec br vib) C.sheng
	where 
		br = 0.25
		vib = 0.01

brightSheng = woodWind' (shortShengSpec br vib) C.sheng
	where 
		br = 1.2
		vib = 0.01

-- hulusi

hulusiSpec bright vib = Wind 
	{ windAtt = 0.12
	, windDec = 0.14
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortHulusiSpec bright vib = Wind 
	{ windAtt = 0.03
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

hulusi = woodWind' (hulusiSpec br vib) C.hulusi
	where 
		br = 0.7
		vib = 0.015

shortHulusi = woodWind' (shortHulusiSpec br vib) C.hulusi
	where 
		br = 0.7
		vib = 0.015

hulusiVibrato = woodWind' (hulusiSpec br vib) C.hulusi
	where 
		br = 0.7
		vib = 0.035

mutedHulusi = woodWind' (hulusiSpec br vib) C.hulusi
	where 
		br = 0.25
		vib = 0.015

brightHulusi = woodWind' (shortHulusiSpec br vib) C.hulusi
	where 
		br = 1.2
		vib = 0.015


-- dizi

diziSpec bright vib = Wind 
	{ windAtt = 0.03
	, windDec = 0.2
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortDiziSpec bright vib = Wind 
	{ windAtt = 0.1
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

dizi = woodWind' (diziSpec br vib) C.dizi
	where 
		br = 0.7
		vib = 0.01

shortDizi = woodWind' (shortDiziSpec br vib) C.dizi
	where 
		br = 0.7
		vib = 0.01

diziVibrato = woodWind' (diziSpec br vib) C.dizi
	where 
		br = 0.7
		vib = 0.035

mutedDizi = woodWind' (diziSpec br vib) C.dizi
	where 
		br = 0.25
		vib = 0.01

brightDizi = woodWind' (shortDiziSpec br vib) C.dizi
	where 
		br = 1.2
		vib = 0.01

------------------------------------
-- x-rays

pulseWidth = Patch
	{ patchInstr = mul (0.75 * 0.6) . at fromMono . mul (fades 0.07 0.1). onCps (uncurry C.pulseWidth)
	, patchFx    = fx1 0.15 smallHall2 }	

xanadu = Patch
	{ patchInstr = mul (1.2 * 0.6) . at fromMono . mul (fades 0.01 2.2). onCps C.xanadu1
	, patchFx    = fx1 0.27 largeHall2 }

alienIsAngry =  Patch
	{ patchInstr = at fromMono . mul (0.5 * fades 0.01 2.3). onCps (C.fmMod 5)
	, patchFx    = fx1 0.15 smallRoom2 }

noiz =  Patch
	{ patchInstr = at fromMono . mul (1.5 * fades 0.01 0.5). onCps C.noiz
	, patchFx    = fx1 0.15 smallHall2 }

blue = Patch
	{ patchInstr = at fromMono . mul (1.5 * fades 0.01 0.5). onCps (C.blue 5 7 0.24 12)
	, patchFx    = fx1 0.25 smallHall2 }	

black = Patch
	{ patchInstr = at fromMono . mul (2 * fades 0.01 0.5). onCps (\cps -> C.black 3 (cps / 2) (cps * 2) 12 (sig cps))
	, patchFx    = fx1 0.25 smallHall2 }

simpleMarimba = Patch
	{ patchInstr = at fromMono . mul (0.8 * fades 0.01 0.5). onCps (C.simpleMarimba 5)
	, patchFx    = fx1 0.25 smallHall2 }
	
impulseMarimba1 = Patch
	{ patchInstr = at fromMono . mul (0.8 * fadeOut 0.75). onCps C.impulseMarimba1
	, patchFx    = fx1 0.3 smallHall2 }

impulseMarimba2 = Patch
	{ patchInstr = at fromMono . mul (0.8 * fadeOut 0.75). onCps C.impulseMarimba2
	, patchFx    = fx1 0.3 smallHall2 }

okComputer = Patch
	{ patchInstr = \(amp, cps) -> (at fromMono . mul (0.75 * sig amp * fades 0.01 0.01) . at (mlp (1500 + sig amp * 8500) 0.1) . (C.okComputer . (/ 25))) (sig cps)
	, patchFx    = fx1 0.25 id }

snowCrackle = Patch
	{ patchInstr = \(amp, cps) -> (return . fromMono . mul (0.8 * sig amp * fades 0.001 0.001) . (C.snowCrackle . (/ 25))) (sig cps)
	, patchFx    = fx1 0.25 id }

noiseBell = Patch 
	{ patchInstr = at fromMono . mul 0.75 . onCps (C.noiseBell (31, 125) 2.3 0.2 . ( * 8))
	, patchFx    = fx1 0.25 smallHall2 }
	
------------------------------------
-- vowels

robotVowels vows latVow = Patch
	{ patchInstr = at fromMono . mul (1.1 * fades 0.1 0.1). onCps (C.vowels 25 vows latVow)
	, patchFx    = fx1 0.15 smallHall2 }

robotLoopVowels loopDur vows = Patch
	{ patchInstr = at fromMono . mul (1.1 * fades 0.1 0.1). onCps (C.loopVowels 25 loopDur vows)
	, patchFx    = fx1 0.15 smallHall2 }

robotVowel vow = Patch
	{ patchInstr = at fromMono . mul (1.1 * fades 0.1 0.1). onCps (C.oneVowel 25 vow)
	, patchFx    = fx1 0.15 smallHall2 }

------------------------------------
-- nature / effects

windWall = Patch
	{ patchInstr = at fromMono . mul (1.25 * fades 0.1 5). onCps C.windWall
	, patchFx    = fx1 0.25 largeHall2 }

mildWind = Patch
	{ patchInstr = at fromMono . mul (1.25 * fades 0.1 1.5). onCps C.mildWind
	, patchFx    = fx1 0.25 largeHall2 }

wind = Patch
	{ patchInstr = at fromMono . mul (0.8 * fades 0.1 1.5). onCps (\cps -> C.thorWind (cps * 2) 150 (0.3, 1))
	, patchFx    = fx1 0.25 largeHall2 }

------------------------------------
-- drums

------------------------------------
-- SHARC patches

-- | Solo instrument.
soloSharc :: SharcInstr -> Patch2
soloSharc instr = Patch 
    { patchInstr = fmap fromMono . onCps (C.soloSharcOsc instr)
    , patchFx    = fx1 0.25 smallHall2
    }

-- | Instrumet played in ensemble (with chorus).
orcSharc :: SharcInstr -> Patch2
orcSharc instr = Patch 
    { patchInstr = fmap fromMono . onCps (C.orcSharcOsc instr)
    , patchFx    = fx1 0.25 largeHall2
    }

-- | Pad orchestra instrument.
padSharc :: SharcInstr -> Patch2
padSharc instr = Patch 
    { patchInstr = fmap fromMono . onCps (C.padSharcOsc instr)
    , patchFx    = fx1 0.35 largeHall2
    }

-- | Pad solo instrument.`
purePadSharc :: SharcInstr -> Patch2
purePadSharc instr = Patch 
    { patchInstr = fmap fromMono . onCps (C.purePadSharcOsc instr)
    , patchFx    = fx1 0.35 largeHall2
    }

-- | Dream Pad patch made with SHARC oscillators.
dreamSharc :: SharcInstr -> Patch2
dreamSharc instr = dreamPadBy (\cps -> C.rndSigSharcOsc instr (ir cps) cps)

-- | Dream Pad patch made with SHARC oscillators.
dreamSharc' :: SharcInstr -> Sig -> Patch2
dreamSharc' instr brightness = dreamPadBy' brightness (\cps -> C.rndSigSharcOsc instr (ir cps) cps) 

