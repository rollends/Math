module Main where

import NelderMead
import Numeric.LinearAlgebra

main :: IO ()
main =
    do
        putStrLn $ show initialSet
        putStrLn $ show minima
        putStrLn $ show (costF minima)
    where
        configuration =
            NelderMeadSettings {
                reflectionFactor    = 1,
                expansionFactor     = 2,
                contractionFactor   = 0.25,
                regenerationFactor  = 0.5,
                costThreshold       = 1e-7,
                volumeThreshold     = 1e-5
            }
        initialSet = [fromList [30, 0], fromList [0, 30], fromList [-30, -30]]
        costF = sinc
        minima = (nelderMead configuration costF initialSet)

sinc x = ((sin . norm_2) x) / (norm_2 x)
