module Main where

import System.Random ( getStdGen, uniformR, RandomGen )
import Data.List ( unfoldr )
import System.Environment ( getArgs )
import Text.Read ( readMaybe )
import Data.Maybe ( fromMaybe, listToMaybe )

-- Pure binomial distribution based on state transformations
binomial
    :: Int              -- ^ Number of trials
    -> (a -> (Bool, a)) -- ^ Outcome state transformation
    -> a                -- ^ Initial state
    -> Int              -- ^ Number of successes
binomial n f a = length . filter id . take n $ unfoldr (Just . f) a

-- Returns True if a point is inside the unit circle
pointInside
    :: Double -- ^ x
    -> Double -- ^ y
    -> Bool
pointInside x y = x^2 + y^2 < 1

samplePoint :: RandomGen g => g -> (Bool, g)
samplePoint g = (pointInside x y, g'')
    where (x, g')  = uniformR (0, 1) g
          (y, g'') = uniformR (0, 1) g'

-- pi estimate
piEst :: RandomGen g => Int -> g -> Double
piEst n g = 4 * fromIntegral m / fromIntegral n
    where m = binomial n samplePoint g

main :: IO ()
main = do
    args <- getArgs
    let n = fromMaybe 10000 $ listToMaybe args >>= readMaybe
    g <- getStdGen
    putStrLn $ "π ≈ " ++ show (piEst n g)
