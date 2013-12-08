import Euterpea
import System.Random
import System.Random.Distributions
import qualified Data.MarkovChain as M

--Step 1: Generate Notes--

sGen :: StdGen
sGen = mkStdGen 42

toAbsP1 :: Float -> AbsPitch
toAbsP1 x = round (40 * x + 30)

mkNote1 :: AbsPitch -> Music Pitch
mkNote1 = note en . pitch

mkLine1 :: [AbsPitch] -> Music Pitch
mkLine1 rands = line (take 128 (map mkNote1 rands))

m4 :: Float -> Float -> Music Pitch
m4 sig mu = let rs1 = rands (gaussian sig mu) sGen
	in mkLine1 (map toAbsP1 rs1)

{-

-- |Melodies tend to move by short distances from note to note.
conjunctMotion :: [Music] -> [Music]
conjunctMotion t = t

-- |Consonant harmonies are preferred to dissonant harmonies, tend to be used at points of musical stability.
acousticConsonance :: [Music] -> [Music]
acousticConsonance t = t

-- |The harmonies in a passage of music tend to be structurally similar to one another.
harmonicConsistency :: [Music] -> [Music]
harmonicConsistency t = t

-- |Tonal music tends to use relatively small macroharmonies, often involving five to eight notes.
limitedMacroharmony :: [Music] -> [Music]
limitedMacroharmony t = t

-- |Over moderate spans of musical time, one note is heard as being more prominent than the others.
centricity :: [Music] -> [Music]
centricity t = t 

-}


-- OLD STUFF --


mel :: [AbsPitch] -> Music Pitch
mel [p1, p2, p3] = hList qn [pitch p1, pitch p2, pitch p3]

hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hList :: Dur -> [Pitch] -> Music Pitch
hList d [] = rest 0
hList d (p:ps) = hNote d p :+: hList d ps

t251 :: Music Pitch
t251 = let 
		dMinor = note wn (D, 4) :=: note wn (F, 4) :=: note wn (A, 4)
		gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
		cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
	in dMinor :+: gMajor :+: cMajor

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let
		chord1 = note d p :=: note d (trans 3 p) :=: note d (trans 7 p)
		chord2 = note d (trans 5 p) :=: note d (trans 9 p) :=: note d (trans 12 p)
		chord3 = note (d*2) (trans (-2) p) :=: note (d*2) (trans 2 p) :=: note (d*2) (trans 5 p) 
	in chord1 :+: chord2 :+: chord3

import Euterpea
import System.Random
import System.Random.Distributions
import qualified Data.MarkovChain as M

sGen :: StdGen
sGen = mkStdGen 42

randInts :: StdGen -> [Int]
randInts g = let (x, g') = next g
	in x: randInts g'

randIntegers :: [Integer]
randIntegers = randomRs (30, 70) sGen

makeLine :: Int -> [Integer]
makeLine n = take n randIntegers

findAfterStar :: [Char] -> Maybe Char
findAfterStar (c:d:r) =
	if c == '*' then Just d
	else findAfterStar (d:r)
findAfterStar _ = Nothing

data Cluster = Cluster SNote [Cluster]
type SNote = (Dur, AbsPitch)

selfSim :: [SNote] -> Cluster
selfSim pat = Cluster (0,0) (map mkCluster pat)
	where mkCluster note =
		Cluster note (map (mkCluster . addMult note) pat)
addMult :: SNote -> SNote -> SNote
addMult (d0, p0) (d1, p1) = (d0 * d1, p0 * p1)

fringe :: Int -> Cluster -> [SNote]
fringe 0 (Cluster note cls) = [note]
fringe n (Cluster note cls) = concatMap (fringe (n - 1)) cls

simToMusic :: [SNote] -> Music Pitch
simToMusic = line . map mkNote
mkNote :: (Dur, AbsPitch) -> Music Pitch
mkNote (d, ap) = note d (pitch ap)

ss pat n tr te =
	transpose tr $ tempo te $ simToMusic $ fringe n $ selfSim pat

m0 :: [SNote]
m0 = [(1,2), (1,0), (1,5), (1,7)]
tm0 = instrument Vibraphone (ss m0 4 50 20)

m1 :: [SNote]
m1 = [(1,0), (0.5,0), (0.5,0)]
tm1 = instrument Percussion (ss m1 4 43 2)

m3 :: [SNote]
m3 = [(hn, 2), (qn, 3), (qn, 0), (hn, 8)]
tm3 = ss m3 4 50 (1/4)
ttm3 = 	let 	l1 = instrument Flute tm3
		l2 = instrument AcousticBass $
			transpose (-9) (revM tm3)
	in l1 :=: l2