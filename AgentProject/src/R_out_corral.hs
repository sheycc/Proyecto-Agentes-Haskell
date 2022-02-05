module R_out_corral where
    import Utils
    import Data.List

    minimalDistance:: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[[Int]]]
    minimalDistance board n m robots_pos childs_pos dirts_pos robots_mask  r0 c0 rf cf = minimalDistance_ board n m robots_pos (length robots_pos) (length robots_pos) childs_pos dirts_pos robots_mask  r0 c0 rf cf

    minimalDistance_:: [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[[Int]]]
    minimalDistance_ board n m robots_pos 0 p childs_pos dirts_pos robots_mask  r0 c0 rf cf = []
    minimalDistance_ board n m robots_pos p0 p childs_pos dirts_pos robots_mask  r0 c0 rf cf = let
        tuple_i = minimalDistance__ board n m r0 c0 rf cf robots_pos (p - p0) childs_pos (length childs_pos) (length childs_pos) robots_mask
        tuple_j = minimalDistance__ board n m r0 c0 rf cf robots_pos (p - p0) dirts_pos (length dirts_pos) (length dirts_pos) robots_mask
        in tuple_i ++ tuple_j ++ minimalDistance_ board n m robots_pos (p0 - 1) p childs_pos dirts_pos robots_mask r0 c0 rf cf


    minimalDistance__:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> [[[Int]]]
    minimalDistance__ board n m r0 c0 rf cf robots_pos i pos 0 p robots_mask = []
    minimalDistance__ board n m r0 c0 rf cf robots_pos i pos p0 p robots_mask = let
        --available_row = (head (robots_pos!!i) - head (pos!!(p - p0))) < 3 && (head (robots_pos!!i) - head (pos!!(p - p0))) > -3 
        --available_column = (robots_pos!!i!!1 - pos!!(p - p0)!!1) < 3 && (robots_pos!!i!!1 - pos!!(p - p0)!!1) > -3
        
        --distance = if available_row && available_column then dfsDistance board n m (robots_pos!!i) (pos!!(p - p0)) 0 [] 0 robots_mask [] else [-1]
        distance = dfsDistance board n m r0 c0 rf cf (robots_pos!!i) (pos!!(p - p0)) 0 [] 0 robots_mask []
        possible_move = (head distance /= -1) && (head distance /= 30)
        in if possible_move then [robots_pos!!i, pos!!(p - p0), distance] : minimalDistance__ board n m r0 c0 rf cf robots_pos i pos (p0 - 1) p ([distance!!1, distance!!2] : robots_mask)
            else minimalDistance__ board n m r0 c0 rf cf robots_pos i pos (p0 - 1) p robots_mask
            
    dfsDistance:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> Int -> [Int] -> Int -> [[Int]] -> [[Int]] -> [Int]
    dfsDistance board n m r0 c0 rf cf pos0 posf d_current direction mask robots_mask route  | (head pos0 == head posf) && (pos0!!1 == posf!!1) = [d_current, head direction, direction!!1]
                                                                                            | (head pos0 > r0) && (head pos0 < rf) && (pos0!!1 > c0) && (pos0!!1 < cf) = [30, head direction, direction!!1]
                                                                                            | containsAny robots_mask pos0 (length robots_mask) (length robots_mask) && d_current == 1 = [30, head direction, direction!!1]
                                                                                            | containsAny route pos0 (length route) (length route) = [30, head direction, direction!!1]
                                                                                            | head pos0 < 0 || head pos0 == n || pos0!!1 < 0 || pos0!!1 == m = [30, head direction, direction!!1]
                                                                                            | (board!!head pos0!!(pos0!!1) == -1) || (board!!head pos0!!(pos0!!1) == 3)  = [30, head direction, direction!!1]
                                                                                            | otherwise = let
                                                                                                dr = [1, 1, -1, -1, 0 , 0, 1, -1]
                                                                                                dc = [1, -1, -1, 1, -1, 1, 0, 0]

                                                                                                runs = if mask == 0 then    [dfsDistance board n m r0 c0 rf cf [head pos0 + head dr, pos0!!1 + head dc] posf (d_current + 1) [ head pos0 + head dr, pos0!!1 + head dc] 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!1, pos0!!1 + dc!!1] posf (d_current + 1) [head pos0 + dr!!1, pos0!!1 + dc!!1] 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!2, pos0!!1 + dc!!2] posf (d_current + 1) [head pos0 + dr!!2, pos0!!1 + dc!!2] 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!3, pos0!!1 + dc!!3] posf (d_current + 1) [head pos0 + dr!!3, pos0!!1 + dc!!3] 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!4, pos0!!1 + dc!!4] posf (d_current + 1) [head pos0 + dr!!4, pos0!!1 + dc!!4] 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!5, pos0!!1 + dc!!5] posf (d_current + 1) [head pos0 + dr!!5, pos0!!1 + dc!!5] 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!6, pos0!!1 + dc!!6] posf (d_current + 1) [head pos0 + dr!!6, pos0!!1 + dc!!6] 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!7, pos0!!1 + dc!!7] posf (d_current + 1) [head pos0 + dr!!7, pos0!!1 + dc!!7] 1 robots_mask (pos0 : route)]
                                                                                                                    else    [dfsDistance board n m r0 c0 rf cf [head pos0 + head dr, pos0!!1 + head dc] posf (d_current + 1) direction 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!1, pos0!!1 + dc!!1] posf (d_current + 1) direction 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!2, pos0!!1 + dc!!2] posf (d_current + 1) direction 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!3, pos0!!1 + dc!!3] posf (d_current + 1) direction 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!4, pos0!!1 + dc!!4] posf (d_current + 1) direction 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!5, pos0!!1 + dc!!5] posf (d_current + 1) direction 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!6, pos0!!1 + dc!!6] posf (d_current + 1) direction 1 robots_mask (pos0 : route),
                                                                                                                            dfsDistance board n m r0 c0 rf cf [head pos0 + dr!!7, pos0!!1 + dc!!7] posf (d_current + 1) direction 1 robots_mask (pos0 : route)]

                                                                                                distances = [head (head runs), head (runs!!1), head (runs!!2), head (runs!!3), head (runs!!4), head (runs!!5), head (runs!!6), head (runs!!7)]
                                                                                                min_dist = minimum distances
                                
                                                                                                in takeSameDistance runs min_dist 8 8
        



    repeatRemove:: [[[Int]]] -> Int -> Int -> [[[Int]]] -> [[[Int]]]
    repeatRemove sort 0 p new_ = []
    repeatRemove sort p0 p new_ = if containsAny__ new_ (sort!!(p - p0)) (length new_) (length new_) || head (sort!!(p - p0)!!2) == 30 then repeatRemove sort (p0 - 1) p new_ 
                                        else sort!!(p - p0) : repeatRemove sort (p0 - 1) p (sort!!(p - p0) : new_)

    robotsPosChild:: [[[Int]]] -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
    robotsPosChild r 0 p childs_pos new_robots = new_robots
    robotsPosChild r p0 p childs_pos new_robots = let
        contains_possible_child = head (r!!(p - p0)!!1) == r!!(p -p0)!!2!!1
                                && r!!(p - p0)!!1!!1 == r!!(p - p0)!!2!!2
                                && childContain (r!!(p - p0)!!1) childs_pos (length childs_pos) (length childs_pos)
       
        in if contains_possible_child then [[r!!(p - p0)!!2!!1, r!!(p - p0)!!2!!2, 1]] ++ new_robots ++ robotsPosChild r (p0 - 1) p childs_pos new_robots
                else [[r!!(p - p0)!!2!!1, r!!(p - p0)!!2!!2, 0]] ++ new_robots ++ robotsPosChild r (p0 - 1) p childs_pos new_robots

    childContain::[Int] -> [[Int]] -> Int -> Int -> Bool
    childContain pos childs_pos 0 p = False
    childContain pos childs_pos p0 p = ((head pos == head (childs_pos!!(p - p0))) && (pos!!1 == childs_pos!!(p - p0)!!1)) 
                                        || childContain pos childs_pos (p0 - 1) p

    robotsPosOldTake:: [[[Int]]] -> Int -> Int -> [[Int]]
    robotsPosOldTake tuple  0 p = []
    robotsPosOldTake tuple p0 p = head (tuple!!(p - p0)) : robotsPosOldTake tuple (p0 - 1) p

    robotsPosNewTake:: [[[Int]]] -> Int -> Int -> [[Int]]
    robotsPosNewTake tuple 0 p = []
    robotsPosNewTake tuple p0 p = [tuple!!(p - p0)!!2!!1, tuple!!(p - p0)!!2!!2] : robotsPosNewTake tuple (p0 - 1) p


    takeRandomPosRobots:: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
    takeRandomPosRobots board n m robots_working robots_pos 0 p robots_random r0 c0 rf cf = []
    takeRandomPosRobots board n m robots_working robots_pos p0 p robots_random r0 c0 rf cf = let
        working = robotIsWorking (robots_pos!!(p - p0)) robots_working (length robots_working) (length robots_working)
    
        new_box = if working then [-1, -1] else takeNewBox (robots_pos!!(p - p0)) board n m robots_random r0 c0 rf cf
        in if new_box == [-1, -1] then takeRandomPosRobots board n m robots_working robots_pos (p0 - 1) p robots_random r0 c0 rf cf
                else [head new_box, new_box!!1, 0] : takeRandomPosRobots board n m robots_working robots_pos (p0 - 1) p (new_box : robots_random) r0 c0 rf cf

    takeNewBox:: [Int] -> [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> Int -> Int -> [Int]
    takeNewBox robot board n m robots_random r0 c0 rf cf = direction_possible
        where
            dr = [1, 1, -1, -1, 0 , 0, 1, -1]
            dc = [1, -1, -1, 1, -1, 1, 0, 0]
            direction_possible = takeDirectionPossible robot dr dc 0 board n m robots_random r0 c0 rf cf 

    takeDirectionPossible:: [Int] -> [Int] -> [Int] -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> Int -> Int -> [Int]
    takeDirectionPossible robot dr dc 8 board n m robots_random r0 c0 rf cf = robot
    takeDirectionPossible robot dr dc p board n m robots_random r0 c0 rf cf = let
        row_0 = head robot + dr!!p
        column_0 = robot!!1 + dc!!p
        available_box = (row_0 < n) && (row_0 >= 0) &&
                        (column_0 < m) && (column_0 >= 0) &&
                        (row_0 < r0 || row_0 > rf || column_0 < c0 || column_0 > cf) &&
                        (board!!row_0!!column_0 == 0) &&
                        not (containsAny robots_random [row_0, column_0] (length robots_random) (length robots_random))
                        
        in if available_box then [row_0, column_0] else takeDirectionPossible robot dr dc (p + 1) board n m robots_random r0 c0 rf cf

    robotIsWorking:: [Int] -> [[Int]] -> Int -> Int -> Bool
    robotIsWorking robot robots_working 0 p = False
    robotIsWorking robot robots_working p0 p = head robot == head (robots_working!!(p - p0)) && robot!!1 == (robots_working!!(p - p0)!!1)
                                                || robotIsWorking robot robots_working (p0 - 1) p

 