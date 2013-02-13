module ESL.LinearRegression where

import Numeric.LinearAlgebra
import Foreign.Storable (Storable)

type Response = Vector Double
type Predicted = Vector Double
type Predictors = Matrix Double
type Betas = Vector Double

data LinearModel = LinearModel{ coefficients :: Betas } deriving (Show)

data LinearModelStats = LinearModelStats{ 
	totalSumOfSquares :: Double,
	sumOfSquaredError :: Double,
	meanSquaredError :: Double,
	rSquared :: Double,
	adjustedRSquared :: Double
} deriving (Show)

ols :: Response -> Predictors -> LinearModel
ols response predictors = LinearModel (predictors <\> response)

predict :: LinearModel -> Predictors -> Predicted
predict model x = x <> (coefficients model)

stats :: LinearModel -> Predictors -> Response -> LinearModelStats
stats model x y = 
	let 	yhat = predict model x
		ybar = constant (meanVector y) (dim y)
		residuals =	y - yhat
		n = rows x
		p = cols x
		sst = sumVector $ (y - ybar) ^ 2
		mst = sst / fromIntegral(n - 1)
		sse = sumVector $ residuals ^ 2
		mse = sse / fromIntegral(p)
		rSquared = 1 - sse / sst
		adjustedRSquared = 1 - ((sse/fromIntegral(n-p)) / (sst/fromIntegral(n-1)))
	in LinearModelStats sst sse mse rSquared adjustedRSquared

sumVector :: (Num a, Storable a) => Vector a -> a
sumVector xs = foldVector (+) 0 xs

meanVector :: Vector Double -> Double
meanVector v = sumVector v / fromIntegral (dim v)
