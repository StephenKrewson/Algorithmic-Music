--Generates a random sequence of notes based on a binomial probability density function--
import Euterpea
import System.Random
import System.Random.Distributions
import qualified Data.MarkovChain as M



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