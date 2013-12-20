module Main where
 
import Data.List (sort)
import Control.Monad (replicateM)
import Control.Monad.Random
 
 
type Chord = [Int]
 
nextChord :: Int -> Chord -> Rand StdGen Chord
nextChord n chord = do
    -- Largest to smallest sort (not the most efficient, but not bad for short lists)
    let sorted = reverse $ sort chord
        m = head sorted -- maximum
    -- Get our random for the largest value
    r <- getRandomR (-n, n)
    -- All numbers can't increase more than `n` over `m`, but can decrease down to 24
    -- We use `sequence` to turn a list of actions in an action that returns a list
    rs <- sequence [getRandomR (24 - x, m + n - x) | x <- tail sorted]
    -- This does quite a lot.  It adds the sorted list together with our random
    -- numbers element-wise, and then clamps all the values to be below 84
    return $ map (min 84) $ zipWith (+) (m:tail sorted) (r:rs)
 
newChord :: Rand StdGen Chord
newChord = replicateM 3 $ getRandomR (24, 84)
 
genChords :: Int -> Rand StdGen [Chord]
genChords n = do
    -- Generate an initial chord (thrown away)
    initial <- newChord
    let loop chord = do
        -- generate the next chord
        next <- nextChord n chord
        -- get all the rest of them (yay lazy evaluation!)
        rest <- loop next
        -- return the whole list
        return $ next : rest
    -- call loop
    loop initial
 
 
main :: IO ()
main = do
    -- evalRandIO automatically provides our Rand monad with a StdGen
    chords <- evalRandIO $ genChords 1
    print $ take 10 chords