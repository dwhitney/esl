module ESL.Examples.Prostate where

import Numeric.LinearAlgebra
import ESL.LinearRegression


cleanMatrix :: Matrix Double -> (Vector Double, Matrix Double)
cleanMatrix m = let [lcavol, lweight, age, lbph, svi, lcp, gleason, pgg45, response, _] = toColumns m 
	in (response, fromColumns [(lcavol ^ 0), lcavol, lweight, age, lbph, svi, lcp, gleason, pgg45])


main = do 
	uncleaned <- fmap readMatrix $ readFile "data/prostate.csv"
	(response, predictors) <- return (cleanMatrix uncleaned)
	putStr $ show (stats (ols response predictors) predictors response)