import Euterpea
import System.Random
import Data.List

jumpSize :: (Num a) => [a] -> [a]
jumpSize (x:[]) = []
jumpSize (x:xs) = abs (x - head xs) : jumpSize xs

--chop 3 $ getPitch 120
chop :: (Ord a) => Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = map sort $ take n xs : chop n (drop n xs)

sGen :: StdGen
sGen = mkStdGen 42

randLetter :: [Char]
randLetter = randomRs ('b', 'm') sGen

--an infinite array!--
randPitch :: [AbsPitch]
randPitch = randomRs (24, 84) sGen

getPitch :: Int -> [AbsPitch]
getPitch n = take n randPitch

mkNote :: AbsPitch -> Music Pitch
mkNote = note qn . pitch

--play $ line $ map mkNote $ getPitch <someval>--

mkChord :: [AbsPitch] -> Music Pitch
mkChord [] = rest 0
mkChord (p:ps) = mkNote p :=: mkChord ps

--play $ mkChord $ getPitch 7--mk


chords1 = filter (\x -> maximum x - minimum x < 11) $ chop 3 $ take 30000 randPitch
constraint2 = instrument RhodesPiano $ line $ map mkChord $ filter (\x -> maximum x - minimum x < 11 && maximum x < 63 && minimum x > 41) $ chop 3 $ take 3000 randPitch
bunch = filter (\x -> (maximum x - minimum x) < 12) $ chop 3 $ getPitch 3000
bunch2 = filter (\x -> maximum x < 80) $ map sort bunch


--play $ line $ map mkChord $ chop 3 $ getPitch 120--
m5 = instrument AcousticBass $ line $ map mkChord $ chop 3 $ getPitch 30
m6 = instrument AcousticBass $ line $ map mkChord $ bunch

m7 = instrument RhodesPiano $ line $ map mkChord $ filter (\x -> 50 < maximum x && maximum x < 70) $ bunch