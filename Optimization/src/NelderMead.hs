module
    NelderMead (
        nelderMead,
        NelderMeadSettings(..),
        Simplex,
        volume,
        centroid
    )
where

import Data.List
import Numeric.LinearAlgebra

type RealVector         = Vector Double
type CostFunction       = RealVector -> Double
type Simplex            = [RealVector]
data NelderMeadSettings =
    NelderMeadSettings {
        reflectionFactor    :: Double,
        expansionFactor     :: Double,
        contractionFactor   :: Double,
        regenerationFactor  :: Double,
        costThreshold       :: Double,
        volumeThreshold     :: Double
    }

nelderMead  :: NelderMeadSettings -> CostFunction -> Simplex -> RealVector
volume      :: Simplex -> Double
centroid    :: Simplex -> RealVector

-- Nelder Mead Optimization Method
nelderMead config costF inSimplex
    | closeEnough && smallEnough                = centroid inSimplex
    | costReflected < head cost                 =
                                    case (costF expanded) < costReflected of
                                        True    -> fNelderMead $ bestN ++ [expanded]
                                        False   -> fNelderMead $ bestN ++ [reflected]
    | costReflected < (cost !! (dimInput - 1))  = fNelderMead $ bestN ++ [reflected]
    | costF contracted < last cost              = fNelderMead $ bestN ++ [contracted]
    | otherwise                                 = fNelderMead fGenSet
    where
        dimInput        = size (head inSimplex)
        simplexCost     = map costF inSimplex
        (cost, x)       = unzip $ sort (zip simplexCost inSimplex)
        bestX           = head x
        worstX          = last x
        bestN           = (take dimInput x)
        centre          = centroid bestN
        reflected       = lerp centre worstX $ -(reflectionFactor $ config)
        expanded        = lerp centre reflected (expansionFactor $ config)
        contracted      = lerp centre worstX (contractionFactor $ config)
        costReflected   = costF reflected
        fGenSet         = [bestX] ++ map ((flip (lerp bestX)) (regenerationFactor $ config)) (tail x)
        fNelderMead     = nelderMead config costF
        closeEnough     = all ((>) (costThreshold config)) simplexCost
        smallEnough     = (volumeThreshold config) > (volume inSimplex)

-- Simplex Helper Functions
centroid set =
    (sum set) / ((scalar . fromIntegral . length) set)

volume (x0:sx) =
    ((abs . det . tr) (fromLists (map vecFromX0 sx))) / scalingF
    where
        vecFromX0   = (curry (toList . (uncurry (flip (-))))) x0
        scalingF    = ((fromIntegral . factorial) $ size x0)

-- Really Shitty Factorial Implementation.
factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial 3 = 6
factorial 4 = 24
factorial 5 = 120
factorial n = n * (factorial $ n - 1)

-- Linear Interpolation. Useful
lerp v1 v2 f = v1 + scale f (v2 - v1)
