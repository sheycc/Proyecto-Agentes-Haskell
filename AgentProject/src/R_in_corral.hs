module R_in_corral where

    import Utils ( containsAny, containsAny__, takeRowColumn, takeSameDistance, takeSequenceCorral, posCorralSort, takeTuple_0, takeNewPosRobot)
    import Data.List ( (\\) )
    
    outCorral :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
    outCorral board n m r0 c0 rf cf robots = minimalDistanceWithoutChildInCorral board n m r0 c0 rf cf robots (length robots) (length robots) []


    minimalDistanceWithoutChildInCorral:: [[Int]] -> Int -> Int ->  Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
    minimalDistanceWithoutChildInCorral board n m r0 c0 rf cf robots 0 p robots_mask = []
    minimalDistanceWithoutChildInCorral board n m r0 c0 rf cf robots p0 p robots_mask = let
        robot = robots!!(p - p0)
        dr = [-2, -2, -2, -2, -2]
        dc = [ 2, -2,  0,  1, -1]

        new_pos = takePosAvailable board n m robot dr dc 0 robots_mask

        in new_pos : minimalDistanceWithoutChildInCorral board n m r0 c0 rf cf robots (p0 - 1) p (new_pos: robots_mask)   

    takePosAvailable :: [[Int]] -> Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> [[Int]] -> [Int]
    takePosAvailable board n m robot dr dc i robots_mask    | i == 5 = []
                                                            | r < 0 || r >= n || c < 0 || c >= m = takePosAvailable board n m robot dr dc (i + 1) robots_mask
                                                            | containsAny robots_mask [r,c] (length robots_mask) (length robots_mask) =  takePosAvailable board n m robot dr dc (i + 1) robots_mask
                                                            | (board!!r!!c == -1) || (board!!r!!c == 1) || (board!!r!!c == 3) =  takePosAvailable board n m robot dr dc (i + 1) robots_mask
                                                            | otherwise = [r,c]
                                                            where
                                                                r = head robot + dr!!i
                                                                c = robot!!1 + dc!!i


    robotsOutCorral:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    robotsOutCorral robots p0 p r0 c0 rf cf | p0 == 0 = []
                                            | r < r0 || r > rf || c < c0 || c > cf = [r, c, 0] : robotsOutCorral robots (p0 - 1) p r0 c0 rf cf 
                                            | otherwise  = [r, c, 3] : robotsOutCorral robots (p0 - 1) p r0 c0 rf cf 
                                            where
                                                r = head (robots!!(p - p0))
                                                c = robots!!(p - p0)!!1

    takePosToMoveRobot :: [[Int]] -> Int -> Int -> [[Int]] -> Int -> [[Int]]
    takePosToMoveRobot board n m position p | p == 8 = []
                                            | otherwise = if available_general then position!!p : takePosToMoveRobot board n m position (p + 1) else takePosToMoveRobot board n m position (p + 1)
                                            where
                                                available_general = (head (position!!p) >= 0) && (head (position!!p) < n) && ((position!!p!!1) >= 0) && ((position!!p!!1) < m) && (board!!head (position!!p)!!(position!!p!!1) /= 1)