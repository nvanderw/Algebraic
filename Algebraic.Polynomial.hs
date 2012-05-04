{-# LANGUAGE ScopedTypeVariables #-}
module Algebraic.Polynomial (
    Polynomial,
    degree) where

import Algebraic
import Data.Array

-- |Ring of polynomials with coefficients in the ring r
data Polynomial r = Polynomial [r]
    deriving (Read, Show, Eq, Ord)

-- |Strips trailing zeros from the list of coefficientse
truncateCoeffs :: (GroupPlus r) => [r] -> [r]
truncateCoeffs = reverse . dropWhile (== gzero) . reverse

instance GroupPlus g => GroupPlus (Polynomial g) where
    (Polynomial xs) >+< (Polynomial ys) = Polynomial $ truncateCoeffs coeffSum 
      where
        zeros = repeat gzero :: [g] -- Infinite list of zeros to add polynomials
        maxLength = max (length xs) (length ys)
        coeffSum = take maxLength $ zipWith (>+<) (xs ++ zeros) (ys ++ zeros)

    gzero = Polynomial []

    gneg (Polynomial xs) = Polynomial $ map gneg xs

-- |Yields the degree of a nonzero polynomial, or Nothing if the polynomial
-- is zero
degree :: Polynomial a -> Maybe Int
degree (Polynomial []) = Nothing
degree (Polynomial xs) = Just (length xs)

instance (Ring r) => Ring (Polynomial r) where
    -- Multiplying by zero should always yield zero
    (Polynomial []) >*< _ = gzero
    _ >*< (Polynomial []) = gzero

    (Polynomial xs) >*< (Polynomial ys) = Polynomial rs
      where
        -- Degree of each input polynomial
        deg1 = length xs - 1
        deg2 = length ys - 1

        -- Maximum degree of resulting polynomial
        resultDeg = deg1 + deg2

        -- Infinite list of zeros to pad the end of the polynomial
        zeros = repeat gzero :: [r]

        -- Pad ends of polynomials up to resulting degree and convert to
        -- arrays
        arr1 = listArray (0, resultDeg) (xs ++ zeros)
        arr2 = listArray (0, resultDeg) (ys ++ zeros)

        -- Function to get the nth coefficient of the resulting polynomial
        getCoeff n = foldl (>+<) gzero $ map
            (\i -> (arr1 ! i) >*< (arr2 ! (n - i))) [0..n]

        -- Coefficients of resulting polynomial
        rs = truncateCoeffs $ map getCoeff [0..(resultDeg)]
