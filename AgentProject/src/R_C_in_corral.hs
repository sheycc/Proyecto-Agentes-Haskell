module R_C_in_corral where

    robotsPosChildUpCorral :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]] 
    robotsPosChildUpCorral board robots p0 p r0 c0 rf cf mask   | p0 == 0 = []
                                                                | otherwise = [head (robots!!(p - p0)), robots!!(p - p0)!!1, 4] : robotsPosChildUpCorral board robots (p0 - 1) p r0 c0 rf cf ((robots!!(p - p0)) : mask)