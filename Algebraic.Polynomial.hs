{-# LANGUAGE ScopedTypeVariables #-}
module Algebraic.Polynomial (
    Polynomial) where

import Algebraic

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
