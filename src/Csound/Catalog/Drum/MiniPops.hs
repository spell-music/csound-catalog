-- |  Drums of the Korg Mini Pops 7 drum machine (recoded from 	Iain McCurdy).
module Csound.Catalog.Drum.MiniPops(
	bass, snare1, snare2, rimShot, cymbal1, cymbal2, bongo1, bongo2, bongo3, 
	claves, cowbell, guiro, maracas, quijada, tamb,

	bass', snare1', snare2', rimShot', cymbal1', cymbal2', bongo1', bongo2', bongo3', 
	claves', cowbell', guiro', maracas', quijada', tamb',
) where

import Csound.Base hiding (guiro) 

rezz cps bw = reson (mpulse 1 0) cps (cps * bw) `withD` 2

bass = bass' 0.43 64

-- dur = 1.7
-- cps = 64
bass' dur cps = aout
	where
		aout = mul (env * 225 * fadeOut dur) $ lp1 500 $ rezz cps 0.001
		env  = transeg [1, dur, -14, 0]


snare1 = snare1' 0.38 800

-- cps = 800
snare1' dur cps = mul (fadeOut dur) $ aout
	where
		anoise = pink
		asig   = fmap (\x -> reson x 6250 9000 `withD` 1) anoise
		aenv   = transeg [1, dur ,-5 , 0]
		asig1  = at (bhp 3000) $ mul aenv asig

		xdur   = 0.006		
		astrike = osc (transeg [cps,xdur,-4,60])
		aenv2 = transeg	[1,xdur,-2,0]
		astrike1 = aenv2 * astrike

		aout = fmap ((0.7 * astrike1) + ) $ mul 2 $ asig1

snare2 = snare2' 0.4 800

snare2' dur cps = mul (fadeOut dur) $ aout
	where
		anoise = pink
		asig   = fmap (\x -> butbp x 5200 5200 `withD` 1) anoise
		aenv   = transeg [1, dur ,-8 , 0]
		asig1  = at (bhp 3000) $ mul aenv asig	

		xdur   = 0.005		
		astrike = osc (transeg [cps,xdur,-4,cps / 4])
		aenv2 = transeg	[1,xdur,-2,0]
		astrike1 = aenv2 * astrike

		aout = fmap ((0.5 * astrike1) + ) $ mul 2.3 $ asig1


rimShot = rimShot' 0.005 1700

-- cps = 1700
-- dur = 0.005
rimShot' dur cps = mul (fadeOut dur) $ asig
	where
		aenv = expon 1 dur 0.0001		
		asig1 = osc' 0.2 cps
		asig2 = reson asig1 cps 1500 `withD` 2
		asig  = bhp 500 (asig1 + asig2 * 0.4 * 0.3)

cymbal1 = cymbal1' 0.304 6000

-- dur = 0.304
-- cps = 6000
cymbal1' dur cps = mul (fadeOut dur) $ do
	anoise <- white
	let asig1 = blp 14000 $ reson	(anoise*aenv) icf (icf*0.7) `withD` 1
	    asig2 = bhp 6000 $ (asig1 + anoise * 0.001)
	return $ 0.25 * aenv * asig2
	where	
		aenv = transeg	[1,dur,-2,0]
		icf = cps


cymbal2 = cymbal2' 1.404 1000

cymbal2' dur cps = mul (fadeOut dur) $ do
	anoise <- white
	let asig = mul aenv $ bhp 6000 $ mul aenv $ lp1 12000 $ reson (anoise * aenv) icf (icf * 0.9) `withD` 1
	return $ astrike * 0.2 + asig * 1.5
	where
		icf = sig $ cps * 5
		aenv = transeg	[1,dur,-2,0]
		xdur = 0.004
		aenv2 = transeg	[1,xdur,-2,0]
		astrike = mul aenv2 $ osc (transeg	[cps,xdur,-4,0.4*cps])


-- dur = 0.2
-- cps = 630
bongo1 = bongo1' 0.2 630

bongo1' dur cps = mul (fadeOut dur) $ asig
	where
		asig = mul (4 * aenv ) $ blp 8000 $ bhp 300 $ rezz cps 0.03
		aenv = transeg	[1,dur,13,0]

bongo2 = bongo2' 0.2 400

-- dur = 0.2
-- cps = 400
bongo2' dur cps = mul (fadeOut dur) $ asig
	where
		kcps =	expon	cps dur (cps * 0.975)
		aenv =	transeg	[1,dur-0.005,0,0.1,0.005,0, 0]
		asig = mul (4 * aenv) $ bhp 100 $ lp1 5000 $ rezz kcps 0.03


bongo3 = bongo3' 1.229 194

-- dur = 1.229
-- cps = 194
bongo3' dur cps = mul (fadeOut dur) $ asig
	where
		aenv  =	transeg	[0, 0.001, -2, 1, dur-0.001, -2, 0]
		kbw   = linseg	[0.05,0.01,0.008]
		asig  = mul (5 * aenv) $ blp 11000 $ rezz cps kbw

claves = claves' 0.186 400

claves' dur cps = aout
	where
		aenv = linseg [1, dur, 0]
		asig1 = rezz cps 0.025
		asig2 = rezz (cps * 5.45) 0.03 
		aout  = mul (3.2 * aenv * fadeOut dur) $ asig1 + 1.3 * asig2

cowbell = cowbell' 0.3 850

cowbell' dur cps = asig
	where
		asig = mul (aenv * 3 * fadeOut dur) $ bhp 100 $
			  rezz cps 0.007
			+ 0.8 * rezz (cps * 5.537) 0.03
		aenv = linseg [1, dur, 0]


guiro = guiro' 0.256 66

guiro' dur cps = asig
	where
		aenv =	linseg	[0,0.001,1,dur-0.111,0.6,0.1,1,0.01,0]
		asig = mul (3 * aenv * fadeOut dur) $ bhp 1000 $ reson (0.1 * sqr kcps) 4300 3000 `withD` 1
		kcps =	transeg	[cps,dur,2,(1.1 * cps)]

maracas = maracas' 0.05 5000

maracas' dur cps = do
	asig <- noise 1 0.04
	return $ mul (0.35 * aenv * fadeOut dur) $ bhp 2000 $ reson asig 9000 4000 `withD` 2
	where
		aenv =	transeg	[1,dur,-4,0]
	

quijada = quijada' 0.817 550

quijada' dur cps = bhp cps $ mul (6 * fadeOut dur) $ phi dur (1/22.7272) + phi (dur * 0.39) (1/13.1579)
	where
		phi dt freq = mul kenv $ reson (mpulse	1 freq) 2727 400 `withD` 1	
			where kenv = transeg	[0.8,0.05,1, 1,dt-0.05,-6,0]


tamb = tamb' 0.271 7000

tamb' dur cps = do
	anoise <- noise 1 0
	return $ mul (1.5 * aenv * fadeOut dur) 
		$ reson (bhp cps $ (+ (anoise * 0.1 * aenv)) $ reson (anoise * aenv) 4600 100 `withD` 2) 9000 3000 `withD` 1
	where aenv = transeg	[1,dur,-8,0]
