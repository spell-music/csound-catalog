module Csound.Catalog.Wave.Vowel(
    -- * Singing a vowel.
    --
    -- | It's best to use this functions with vibrato.
    --
    -- > vibrato 0.12 5 $ oneVowel maleA 330

    vowels, loopVowels, oneVowel, Vowel,
    
    -- * Vowels
    maleA, maleE, maleIY, maleO, maleOO, maleU, maleER, maleUH,
    femaleA, femaleE, femaleIY, femaleO, femaleOO    
) where

import Data.List(transpose)

import Csound.Base

-- | Sings a sequence of vowels with the given frequency. 
--
-- > vowels [(vowel1, dur1), (vowel2, dur2), (vowel3, dur3), ...] lastVowel cps
--
-- * @vowel1@, @vowel2@, ... lastVowel -- vowels
--
-- * dur1, dur2, ... - durations
--
-- * cps - frequency of the note.
vowels :: [(Vowel, D)] -> Vowel -> Sig -> Sig
vowels = vowelsBy mkEnv
    where mkEnv xs fin = linseg ( ( ++ [fin, 1, fin]) $ (\(a, b) -> [a, b]) =<< xs)

-- | Sings a loop of vowels with the given frequency. 
--
-- > loopVowels xdur [(vowel1, dur1), (vowel2, dur2), (vowel3, dur3), ...] cps
--
-- * xdur - the duration of the loop of vowels.
--
-- * @vowel1@, @vowel2@, ...  -- vowels
--
-- * dur1, dur2, ... - durations
--
-- * cps - frequency of the note.
loopVowels :: Sig -> [(Vowel, D)] -> Sig -> Sig
loopVowels xdur params = vowelsBy mkEnv params lastVowel
    where 
        mkEnv xs fin = loopseg (1 / xdur) 0 0 ((++ [sig fin]) $ (\(a, b) -> [sig a, sig b]) =<< xs)
        lastVowel = fst $ head params

-- | Generic construcotr for the signals that interpolate between vowel sounds.
-- It takes a function that constructs an envelope to proceed from one vowel to another.
-- The envelope function takes two parameters. It's list of vowels with durations
-- and the value of the final vowel. 
--
-- > vowelsBy makeEnvelope vowelSquence lastVowel cps
vowelsBy :: ([(D, D)] -> D -> Sig) -> [(Vowel, D)] -> Vowel -> Sig -> Sig
vowelsBy mkEnv params lastVowel cps = case params of
    [(vow, _)] -> oneVowel vow cps
    _          -> (/100) $ sum $ zipWith3 harm 
                        [fmt1, fmt2, fmt3, fmt4, fmt5]
                        [amp1, amp2, amp3, amp4, amp5]
                        [bw1,  bw2,  bw3,  bw4,  bw5]
    where
        (vs, dts) = unzip params
        [ fmt1, amp1, bw1, fmt2, amp2, bw2, fmt3, amp3, bw3
            , fmt4, amp4, bw4, fmt5, amp5, bw5, ris, dur, dec
            ] = zipWith (\xs lastV -> mkEnv (zip xs dts) lastV) (transpose $ fmap vowelParams vs) (vowelParams lastVowel)

        harm fmt amp bw = fof amp cps fmt ioct bw ris dur dec iolaps sine sigmoid idur `withDs` [0, 1]
        ioct = 0
        iolaps = 20 


-- | Sings a single vowel with the given frequency.
oneVowel :: Vowel -> Sig -> Sig
oneVowel v cps = (/100) $ sum $ zipWith3 harm
                        [fmt1, fmt2, fmt3, fmt4, fmt5]
                        [amp1, amp2, amp3, amp4, amp5]
                        [bw1,  bw2,  bw3,  bw4,  bw5]
    where
        [ fmt1, amp1, bw1, fmt2, amp2, bw2, fmt3, amp3, bw3
            , fmt4, amp4, bw4, fmt5, amp5, bw5, ris,  dur,  dec         
            ] = vowelParams v

        harm fmt amp bw = fof (sig amp) cps (sig fmt) ioct (sig bw) (sig ris) (sig dur) (sig dec) iolaps sine sigmoid idur `withDs` [0, 1]
        ioct = 0
        iolaps = 20 
     

vowelParams :: Vowel -> [D]
vowelParams v = fmap (flip tableD vowelTab . (+ index)) [0 .. 17] 
    where index = vowelIndex v
        
-- | Abstract type that represents a vowel. 
newtype Vowel = Vowel { unVowel :: D }

instance Arg Vowel where
    argMethods = makeArgMethods Vowel unVowel

maleA, maleE, maleIY, maleO, maleOO, maleU, maleER, maleUH,
    femaleA, femaleE, femaleIY, femaleO, femaleOO :: Vowel

maleA       = Vowel 0;      maleE       = Vowel 1;      maleIY      = Vowel 2
maleO       = Vowel 3;      maleOO      = Vowel 4;      maleU       = Vowel 5
maleER      = Vowel 6;      maleUH      = Vowel 7;      femaleA     = Vowel 8
femaleE     = Vowel 9;      femaleIY    = Vowel 10;     femaleO     = Vowel 11
femaleOO    = Vowel 12

vowelIndex :: Vowel -> D
vowelIndex = (* 18) . unVowel

vowelTab :: Tab
vowelTab = skipNorm $ doubles
-- 1 - male voice singing A
--	  fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	[ 609,	0,	    100,	1000,	-6,	    100,	2450,	-12,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 2700,	-11,	100,	3240,	-24,	100,	0.003,	0.02,	0.007
-- 2 - male voice singing E
--	fmt1	amp1	bw1	    fmt2	amp2	bw2 	fmt3	amp3	bw3
	, 400,	0,	    100,	1700,	-9,	    100,	2300,	-8,	    100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 2900,	-11,	100,	3400,	-19,	100,	0.003,	0.02,	0.007
-- 3 - male voice singing IY
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 238,	0,	    100,	1741,	-20,	100,	2450,	-16,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 2900,	-20,	100,	4000,	-32,	100,	0.003,	0.02,	0.007
-- 4 - male voice singing O
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 325,	0,	    100,	700,	-12,	100,	2550,	-26,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 2850,	-22,	100,	3100,	-28,	100,	0.003,	0.02,	0.007
-- 5 - male voice singing OO
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 360,	0,	    100,	750,	-12,	100,	2400,	-29,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	ilris	ildur	ildec
	, 2675,	-26,	100,    2950,	-35,	100,	0.003,	0.02,	0.007
-- 6 - male voice singing U
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 415,	0,	    100,	1400,	-12,	100,	2200,	-16,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 2800,	-18,	100,	3300,	-27,	100,    0.003,	0.02,	0.007
-- 7 - male voice singing ER
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 300,	0,	    100,	1600,	-14,	100,	2150,	-12,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 2700,	-15,	100,	3100,	-23,	100,	0.003,	0.02,	0.007
-- 8 - male voice singing UH
-- 	fmt1	amp1	bw1	    fmt2	amp2	bw2 	fmt3	amp3	bw3
	, 400, 	0,  	100,	1050,	-12,	100,	2200,	-19,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 2650,	-20,	100,	3100,	-29,	100,	0.003,	0.02,	0.007
-- 9 - female voice singing A
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 650,	0,	    100,	1100,	-8,	    100,	2860,	-13,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 3300,	-12,	100,	4500,	-19,	100,	0.003,	0.02,	0.007
-- 10 - female voice singing E
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 500,	0,	    100,	1750,	-9, 	100,	2450,	-10,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 3350,	-14,	100,	5000,	-23,	100,	0.003,	0.02,	0.007
-- 11 - female voice singing IY
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 330, 	0,	    100,	2000,	-14,	100,	2800,	-11,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 3450,	-50,	100,	4500,	-52,	100,	0.003,	0.02,	0.007
-- 12 - female voice singing O
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 400,  0,	    100,	840,	-12,	100,	2800,	-26,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 3250,	-24,	100,	4500,	-31,	100,	0.003,	0.02,	0.007
-- 13 - female voice singing OO
--	fmt1	amp1	bw1	    fmt2	amp2	bw2	    fmt3	amp3	bw3
	, 280,	0,	    100,	650,	-18,	100,	2200,	-48,	100
--	fmt4	amp4	bw4	    fmt5	amp5	bw5	    ilris	ildur	ildec
	, 3450,	-50,	100,	4500,	-52,	100,	0.003,	0.02,	0.007
    ]

