import Euterpea
import System.Random
import Data.List

--two insights were: (1) move "take" fx to start of expression, don't waste time generating--
--pattern match n (x:y:zs) to get two indexes in filter 


jumpSize' :: (Num a) => [a] -> [a]
jumpSize' (x:[]) = []
jumpSize' (x:xs) = abs (x - head xs) : jumpSize' xs

--takes a list of lists, finds difference between max value of current chord and following chord, prepends that delta--
jumpSize :: (Num a, Ord a) => [[a]] -> [[a]]
jumpSize (_:[]) = []
jumpSize (x:y:[]) = (abs (maximum x - maximum y) : x) : []
jumpSize (x:y:zs) = (abs (maximum x - maximum y) : x) : jumpSize (y:zs)

jumpSize1 :: (Num a, Ord a) => [[a]] -> [[a]]
jumpSize1 (_:[]) = []
jumpSize1 (w:x:y:[]) = (abs (maximum x - maximum w) : x ++ [abs (maximum x - maximum y)]) : []
jumpSize1 (w:x:y:zs) = (abs (maximum x - maximum w) : x ++ [abs (maximum x - maximum y)]) : jumpSize (x:y:zs)


--maybe make this even more recursive? or find a way to use jumpSize to find the strings of 222222222--
--i can probably get rid of the 42 - 62 filter once this is working: because overdetermination--
--still useful to filter out huge leaping chords--

test1 = [[1,2,3], [3,5,6], [7,5,4], [2,9,10]]

jumpRecur :: (Num a, Ord a) => a -> [[a]] -> [[a]]
jumpRecur n (xs)
	| [x | x <- xs, head x > n] == []	= xs
	| otherwise 						= jumpRecur n $ jumpSize $ filter (\x -> head x <= n) xs



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

--constrain total size of the chord: no huge spans!--
filterSize n xs = [x | x <- xs, maximum x - minimum x < n]
--constrain chord space to absPitch range 42 - 62--
filterRange xs = [x | x <- xs, minimum x > 41 && maximum x < 63]
--can also be written as lambda functions with 'filter'--


--Here are the melodies, in order of increasing tonality constraints--
m1 = instrument RhodesPiano $ line $ map mkChord $ take 10 $ chop 3 $ randPitch
m2 = line $ map mkChord $ take 10 $ filterSize 11 $ chop 3 $ randPitch
m3 = line $ map mkChord $ take 10 $ filterSize 11 $ filterRange $ chop 3 $ randPitch
