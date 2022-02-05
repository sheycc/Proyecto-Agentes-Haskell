module R_C_out_corral where
    import Utils ( containsAny, takeTuple_0, takeTuple_1, takePosToMove, posCorralSort, takePosCorralAvailables, takeSameDistance )
    import Data.List
    import DFS ( nextPosition__ )

    minimalDistanceWithChild:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
    minimalDistanceWithChild board n m r0 c0 rf cf robots_up robots_mask = let
        corral_availables = takePosCorralAvailables board (length robots_up) (r0 + 1) (c0 + 1) r0 c0 rf cf
        dr = [ 1, 1, 1, 1, 1, 2, 2, 2, 2, 2]
        dc = [0, 1, -1, 2,-2, 0, 1,-1, 2,-2]
        robots_gradient = minimalDistance_  board n m robots_up (length robots_up) (length robots_up) robots_mask dr dc
        in robots_gradient
        --in corral_availables

    minimalDistance_:: [[Int]] -> Int -> Int ->[[Int]] -> Int -> Int -> [[Int]] -> [Int] -> [Int] -> [[Int]]
    minimalDistance_ board n m robots_pos p0 p robots_mask dr dc    | p0 == 0 = []
                                                                    | otherwise = let
                                                                        robot = robots_pos!!(p - p0)
                                                                        x = minimalDistance__ board n m robot robots_mask dr dc 0 n []
                                                                        take_min = if null x then robot else x
                                                                        in take_min : minimalDistance_ board n m robots_pos (p0 - 1) p (take_min : robots_mask) dr dc


    minimalDistance__ :: [[Int]] -> Int -> Int -> [Int] -> [[Int]] -> [Int] -> [Int] -> Int -> Int -> [Int] -> [Int]                                                                     
    minimalDistance__ board n m robot robots_mask dr dc i min current   | i == 10 = current
                                                                        | r < 0 || r >= n || c < 0 || c >= m = minimalDistance__ board n m robot  robots_mask dr dc (i + 1) min current
                                                                        | (board!!r!!c /= 0) && (board!!r!!c /= 2) = minimalDistance__ board n m robot  robots_mask dr dc (i + 1) min current
                                                                        | containsAny robots_mask [r,c] (length robots_mask) (length robots_mask) = minimalDistance__ board n m robot robots_mask dr dc (i + 1) min current
                                                                        | otherwise = let
                                                                            new_min = minimum [min, d]
                                                                            new_current = if new_min < min then [r,c] else current

                                                                            in minimalDistance__ board n m robot robots_mask dr dc (i + 1) new_min new_current
                                                                        where
                                                                            r = head robot + dr!!i
                                                                            c = robot!!1 + dc!!i
                                                                            d = abs (r - (n - 1))
                                                                            bound_corral = board!!r!!c == -1

--    minimalDistance___ :: [[Int]] -> Int -> Int -> [Int] -> [Int] -> [[Int]] -> [Int] -> [Int] -> Int -> [[Int]]                                                                      
--    minimalDistance___ board n m robot pos robots_mask dr dc i   | i == 8 = [] 
--                                                                | r < 0 || r >= n || c < 0 || c >= m = minimalDistance___ board n m robot pos robots_mask dr dc (i + 1)
--                                                                | (board!!r!!c == -1) || (board!!r!!c == 3) || (board!!r!!c == 1) = minimalDistance___ board n m robot pos robots_mask dr dc (i + 1)
--                                                                | containsAny robots_mask [r,c] (length robots_mask) (length robots_mask) = minimalDistance___ board n m robot pos robots_mask dr dc (i + 1)
--                                                                | otherwise = let
--                                                                    in [d, r, c] : minimalDistance___ board n m robot pos robots_mask dr dc (i + 1)
--                                                                where
--                                                                    r = head robot + dr!!i
--                                                                    c = robot!!1 + dr!!i
--                                                                    d = abs (head pos - r) + abs (pos!!1 - c)
    
    possibleDown :: [[Int]] -> Int -> Int -> Int -> Int -> [Int]
    possibleDown board n m r c  | ((r + 1) == (n - 1)) && (board!!(r + 1)!!c == 0) = [r + 1, c]
                                | ((r + 1) == (n - 1)) && ((c + 1) < m) && (board!!(r + 1)!!(c + 1) == 0) = [r + 1, c + 1]
                                | ((r + 1) == (n - 1)) && ((c - 1) > 0) && (board!!(r + 1)!!(c - 1) == 0) = [r + 1, c - 1]
                                | ((r + 1) == (n - 1)) && ((c + 2) < m) && (board!!(r + 1)!!(c + 2) == 0) = [r + 1, c + 2]
                                | ((r + 1) == (n - 1)) && ((c - 2) > 0) && (board!!(r + 1)!!(c - 2) == 0) = [r + 1, c - 2]
                                | otherwise = []
                        

    takePosWithDist :: [[Int]] -> Int -> Int -> Int -> [Int]
    takePosWithDist runs d p0 p | p0 == 0 = []
                                | d == dist = [r,c]
                                | otherwise = takePosWithDist runs d (p0 - 1) p
                                where
                                    t = runs!!(p - p0)
                                    dist = head t
                                    r = t!!1
                                    c = t!!2


    robotsPosChildUp :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] 
    robotsPosChildUp robots p0 p r0 c0 rf cf    | p0 == 0 = []
                                                | head (robots!!(p - p0)) > r0 = [head (robots!!(p - p0)), robots!!(p - p0)!!1, 2] : robotsPosChildUp robots (p0 - 1) p r0 c0 rf cf
                                                | otherwise = [head (robots!!(p - p0)), robots!!(p - p0)!!1, 1] : robotsPosChildUp robots (p0 - 1) p r0 c0 rf cf
    
    addPosCorralAvailables:: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
    addPosCorralAvailables board pos p0 p   | p0 == 0 = []
                                            | otherwise = let
                                                c = pos!!(p - p0)
                                                in if board!!head c!!(c!!1) == 0 then c : addPosCorralAvailables board pos (p0 - 1) p else addPosCorralAvailables board pos (p0 - 1) p