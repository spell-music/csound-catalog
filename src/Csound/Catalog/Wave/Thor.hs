{-# Language FlexibleContexts #-}
module Csound.Catalog.Wave.Thor(
	cathedralOrgan, cathedralOrganFx, hammondOrgan,

	amPiano, 

	pwBass, pwHarpsichord, pwEnsemble,

	simpleBass, 

	EpianoOsc(..), epiano, 

	noisyChoir, thorWind, mildWind, boom, windWall, 

	razorPad, razorLead
) where

import Data.List
import Control.Monad

import Csound.Base 

-- some instruments from the Thor explained series
--
-- https://www.propellerheads.se/substance/discovering-reason/index.cfm?article=part19&fuseaction=get_article

------------------------------
-- thor oscillators

------------------------------
-- 1 oscillators

cathedralOrganFx :: Sig -> Sig2
cathedralOrganFx = mixAt 0.25 largeHall . fromMono

cathedralOrgan cps = mul 0.3 $ sum $ fmap ($ cps) [hammondOrgan 3 , detune (2 * cent 4) (hammondOrgan 10), detune (3 * cent 3) (hammondOrgan 6)]

-- | hammondOrgan detune
--
-- detune = [0, 30] (in cents)
hammondOrgan :: Sig -> Sig -> SE Sig
hammondOrgan dt x = mul (fades 0.01 0.05) $ fmap mean $ mapM rndOsc 
	[ x
	, 2 * x * cent dt
	, 3 * x * cent (2 * dt) ]

------------------------------
-- 2 am & sync

amPiano :: Sig -> SE Sig
amPiano x = mul env $ at (mlp (env * (3000 + x)) 0.25) $ (rndSaw x * rndSaw (4 * x))
	where env = leg 0.01 4 0 0.02

------------------------------
-- 3 pwm

pwBass :: Sig -> SE Sig
pwBass cps = mul (fades 0.005 0.05) $ at (mlp 1500  0.1) $ rndPw (0.25 * (1 + 0.07 * osc (1 + (7 * cps / 1000)))) cps

simpleBass :: (D, D) -> Sig
simpleBass (amp, cps') = aout
	where
		cps = sig cps'

		all = sum 
			[ 0.4 * oscBy pulse $ cps * 0.998 - 0.12
			, 0.4 * osc         $ cps * 1.002 - 0.12
			, 0.4 * oscBy pulse $ cps * 0.998 - 0.12 
			, 0.7 * osc         $ cps         - 0.24 ]

		aout = mul (kgain * sig amp * linsegr [0, 0.01, 1, (3.5 * amp), 0] 0.35 0)
			$ blp (700 + (sig amp * 500))
			$ bhp 65
			$ bhp 65
			$ blp ksweep
			$ blp ksweep all

		ksweep = expsegr [3000, 0.03, 9000] 3 1 - 3000

		pulse = sines [1, 1, 1, 1, 0.7, 0.5, 0.3, 0.1]

		kgain = 2

pwHarpsichord :: Sig -> SE Sig
pwHarpsichord x = mul 2.5 $ mul (leg 0.005 1.5 0 0.25) $ at (mlp (env * 8000) 0.15) $ at (hp 2500 0.3) $ rndPw 0.4 x
	where env = leg 0.01 4 0 0.01

pwEnsemble :: Sig -> SE Sig
pwEnsemble x = mul 0.3 $ at (mlp (3500 + x * 2) 0.1) $ mul (leg 0.5 0 1 1) $ sum
	[ f 0.2 0.11 2 (x * cent (-6))
	, f 0.8 (-0.1) 1.8 (x * cent 6)
	, f 0.2 0.11 2 (x * 0.5) ]
	where f a b c = rndPw (a + b * tri c)

------------------------------
-- 4 Multi osc (unision)

data EpianoOsc = EpianoOsc 
	{ epianoOscChorusNum :: Int
	, epianoOscChorusAmt :: Sig
	, epianoOscNum       :: Sig	
	, epianoOscWeight    :: Sig
	}

epiano :: [EpianoOsc] -> (D, D) -> SE Sig
epiano xs (amp, cps) = mul (sig amp * leg 0.001 sust 0 rel) $ at (mlp (2500 + 4500 * (leg 0.085 3 0 0.1)) 0.25) $
	fmap sum $ mapM (\x -> mul (epianoOscWeight x) $ multiRndSE (epianoOscChorusNum x) (epianoOscChorusAmt x) (detune (epianoOscNum x) rndOsc) (sig cps)) xs
 -- (multiRndSE 4 5 rndOsc + multiRndSE 8 10 (detune (2.01) rndOsc))
 	where 
 		sust = amp + 2 + (0.7 - 3 * k ** 2)
 		rel  = (amp / 10) + 0.05 - (k / 10)
 		k    = cps / 1000

------------------------------
-- 5 noise

-- noisyChor numberOfFilters bandWidthRatio cps
noisyChoir :: Int -> Sig -> Sig -> SE Sig
noisyChoir n ratio cps = mul 0.5 $ genGhostChoir white [1, 1] [1, 0.5] n (5 + 300 ** ratio) cps

genGhostChoir :: (SE Sig) -> [Sig] -> [Sig] -> Int -> Sig -> Sig -> SE Sig
genGhostChoir noiseGen amps hs n bw cps = mul env $ fmap sum $ zipWithM f amps hs
	where 
		f :: Sig -> Sig -> SE Sig
		f a h = mul a $ bat (filt n bp (h * cps) bw) noiseGen
		env = fades 0.4 0.5

------------------------------
-- 6 noise

mildWind :: Sig -> SE Sig
mildWind cps = thorWind (cps * 2) 120 (0.2, 0.5)

thorWind :: Sig -> Sig -> (Sig, Sig) -> SE Sig 
thorWind cps bw (speedMin, speedMax) = mul 1.3 $ do 
	speed <- rspline (-1) 1 speedMin speedMax
	at (mlp (cps + bw * speed) 0.8) pink 

boom :: Sig -> SE Sig
boom cps = mul (1.2 * expon 1 2.05 0.001) $ fmap sum $ mapM (\x -> bat (bp (0.5 * cps * x) 10) white) [1, 1.51, 2.1, 3.05]

windWall :: Sig -> SE Sig
windWall cps = mul amEnv $ at (hp1 400) $ at (mlp (filtEnv * cps) 0.2) (mul 20 white )
	where 
		amEnv   = leg 7 10 0 8
		filtEnv	= leg 6 0 1 5

------------------------------
-- 9, 10 fm

razorPad speed amp cps = f cps + 0.75 * f (cps * 0.5)
	where f cps = mul (leg 0.5 0 1 1) $ genRazor (filt 1 mlp) speed amp cps

razorLead bright speed amp cps = mul (0.5 * leg 0.01 1 0.5 0.5) $ genRazor (filt 2 (lp18 $ 2 * bright)) speed amp cps

genRazor filter speed amp cps = mul amp $ do
	a1 <- ampSpline 0.01
	a2 <- ampSpline 0.02	

	return $ filter (1000 + 2 * cps + 500 * amp) 0.1 $ mean [
		  fosc 1 3 (a1 * uosc (speed)) cps
		, fosc 3 1 (a2 * uosc (speed + 0.2)) cps
		, fosc 1 7 (a1 * uosc (speed - 0.15)) cps ]
	where ampSpline c = rspline ( amp) (3.5 + amp) ((speed / 4) * (c - 0.1)) ((speed / 4) * (c  + 0.1))




