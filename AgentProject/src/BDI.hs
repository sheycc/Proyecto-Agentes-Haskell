module BDI where

    import Utils    (generateNewBoard,
                    sortGradient,
                    sortTuple,
                    takePos,
                    takePosChild,
                    takeChildsInCorral,
                    takePosRobots,
                    takePosCorralAvailables )
    import Data.List
    import R_in_corral
    import R_C_in_corral    (robotsPosChildUpCorral )
    import R_C_out_corral   (minimalDistanceWithChild, 
                            robotsPosChildUp )
    import R_out_corral (minimalDistance,
                        repeatRemove,
                        robotsPosChild,
                        robotsPosNewTake,
                        robotsPosOldTake,
                        takeRandomPosRobots )
    

    bdiStart:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[[Int]]]
    bdiStart board n m r0 c0 rf cf robots_up = let

        --old information
        childs_pos = takePosChild board n m r0 c0 rf cf
        robots_pos = takePosRobots board n m r0 c0 rf cf 

        dirts_pos =  takePos board n m 2

        childs_add = takeChildDown robots_up (length robots_up) (length robots_up)
        childs_add_sort = sortTuple childs_add
        robots_up_chains = robotsUpChainsTo3 robots_up (length robots_up) (length robots_up)

        new_pos_robots= homeworkRobot board n m robots_pos childs_pos dirts_pos robots_up_chains r0 c0 rf cf childs_add_sort
        
        --new information
        new_board = generateNewBoard board n m childs_add_sort new_pos_robots        
        new_robots_up = robotsUpCreate new_pos_robots (length new_pos_robots) (length new_pos_robots)

        in [new_board, new_robots_up]
    
    takeChildDown::[[Int]] -> Int -> Int -> [[Int]]
    takeChildDown robots p0 p   | p0 == 0 || p == 0 = []
                                | robots!!(p - p0)!!2 == 4 = [head (robots!!(p - p0)), robots!!(p - p0)!!1] : takeChildDown robots (p0 - 1) p
                                | otherwise = takeChildDown robots (p0 - 1) p

    robotsUpChainsTo3:: [[Int]] -> Int -> Int -> [[Int]]
    robotsUpChainsTo3 robots p0 p   | p0 == 0 || p == 0 = []
                                    | robots!!(p - p0)!!2 == 4 = [head (robots!!(p - p0)), robots!!(p - p0)!!1, 3] : robotsUpChainsTo3 robots (p0 - 1) p
                                    | otherwise = robots!!(p - p0) : robotsUpChainsTo3 robots (p0 - 1) p


    robotsUpCreate:: [[Int]] -> Int -> Int -> [[Int]]
    robotsUpCreate robots p0 p  | p0 == 0 = []
                                | robots!!(p - p0)!!2 == 0 = robotsUpCreate robots (p0 - 1) p
                                | otherwise = robots!!(p - p0) : robotsUpCreate robots (p0 - 1) p

    robotsWithChildRemove:: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
    robotsWithChildRemove robots_up robots_pos 0 p result = result
    robotsWithChildRemove robots_up robots_pos p0 p result = let
        contain = robotContain (robots_pos!!(p - p0)) robots_up (length robots_up) (length robots_up)
        in if contain then robotsWithChildRemove robots_up robots_pos (p0 - 1) p result
            else [robots_pos!!(p - p0)] ++ result ++ robotsWithChildRemove robots_up robots_pos (p0 - 1) p result

    robotContain:: [Int] -> [[Int]] -> Int -> Int -> Bool
    robotContain robot robot_up 0 p = False
    robotContain robot robot_up p0 p = p > 0 && (head robot == head (robot_up!!(p - p0)) && robot!!1 == robot_up!!(p - p0)!!1) ||
                                            robotContain robot robot_up (p0 - 1) p

    homeworkRobot:: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
    homeworkRobot board n m robots_pos childs_pos dirts_pos robots_up r0 c0 rf cf childs_add = let
        -- (r,c,1) -> robot con ninno fuera del corral
        -- (r,c,2) -> robot con ninno dentro del corral
        -- (r,c,4) -> era (r, c, 2) y llega a la posicion donde deja al ninno -> (r, c, 3)
        -- (r,c,3) -> esta en el corral sin ninno e intenta salir del corral
        
        robots_without_child_in_corral_new : r_without_c_with_c : _ = robotsWithoutChildInCorralSolve board robots_up r0 c0 rf cf n m 
        robots_with_child_in_corral_new : r_with_c_with_c :_ = robotsWithChildInCorralSolve board robots_up r0 c0 rf cf n m r_without_c_with_c
        robots_with_child_without_corral_new: r_whit_c_without_c:_ = robotsWithChildWithoutCorralSolve board robots_up r0 c0 rf cf n m (r_with_c_with_c ++ r_without_c_with_c)
        robots_without_child_without_corral_new = robotsWithoutChildWithoutCorralSolve board n m robots_pos childs_pos dirts_pos r0 c0 rf cf (r_whit_c_without_c ++ r_with_c_with_c ++ r_without_c_with_c)
        
        in robots_without_child_in_corral_new ++ robots_with_child_in_corral_new ++ robots_with_child_without_corral_new ++ robots_without_child_without_corral_new

    takeRobots:: [[Int]] -> Int -> Int -> Int -> [[Int]]
    takeRobots robots_up value p0 p | p0 == 0 = []
                                    | robots_up!!(p - p0)!!2 == value = [head (robots_up!!(p - p0)), robots_up!!(p - p0)!!1] : takeRobots robots_up value (p0 - 1) p
                                    | otherwise = takeRobots robots_up value (p0 - 1) p

    takeRobots1:: [[Int]] -> Int -> Int -> Int -> [[Int]]
    takeRobots1 robots_up value p0 p | p0 == 0 = []
                                    | robots_up!!(p - p0)!!2 == value = robots_up!!(p - p0) : takeRobots1 robots_up value (p0 - 1) p
                                    | otherwise = takeRobots1 robots_up value (p0 - 1) p


    robotsWithoutChildInCorralSolve :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[[Int]]]
    robotsWithoutChildInCorralSolve board robots_up r0 c0 rf cf n m = let

        robots_without_child_in_corral_0 = takeRobots robots_up 3 (length robots_up) (length robots_up)
        robots_without_child_in_corral = outCorral board n m r0 c0 rf cf robots_without_child_in_corral_0
        robots_without_child_in_corral_new = robotsOutCorral robots_without_child_in_corral (length robots_without_child_in_corral) (length robots_without_child_in_corral) r0 c0 rf cf

        in [robots_without_child_in_corral_new, robots_without_child_in_corral]
    
    robotsWithChildInCorralSolve:: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[[Int]]]
    robotsWithChildInCorralSolve board robots_up r0 c0 rf cf n m robots_mask = let

        robots_with_child_in_corral_0 = takeRobots robots_up 2 (length robots_up) (length robots_up)
        robots_with_child_in_corral_new = robotsPosChildUpCorral board robots_with_child_in_corral_0 (length robots_with_child_in_corral_0) (length robots_with_child_in_corral_0) r0 c0 rf cf []
        
        in [robots_with_child_in_corral_new , robots_with_child_in_corral_0]
    
    robotsWithChildWithoutCorralSolve:: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[[Int]]]
    robotsWithChildWithoutCorralSolve board robots_up r0 c0 rf cf n m robots_mask = let
        
        robots_with_child_without_corral_0 = takeRobots robots_up 1 (length robots_up) (length robots_up)
        --robots_with_child_without_corral_1 = takeRobots1 robots_up 1 (length robots_up) (length robots_up)
        robots_with_child_without_corral = minimalDistanceWithChild board n m r0 c0 rf cf robots_with_child_without_corral_0 robots_mask
        robots_with_child_without_corral_new = robotsPosChildUp robots_with_child_without_corral (length robots_with_child_without_corral) (length robots_with_child_without_corral) r0 c0 rf cf
        in [robots_with_child_without_corral_new, robots_with_child_without_corral]
        --in [robots_with_child_without_corral_0, robots_with_child_without_corral_1]
    

    robotsWithoutChildWithoutCorralSolve:: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
    robotsWithoutChildWithoutCorralSolve board n m robots_pos childs_pos dirts_pos r0 c0 rf cf robots_mask = let
        robots_gradient = minimalDistance board n m robots_pos childs_pos dirts_pos robots_mask r0 c0 rf cf
        sort__ = sortGradient robots_gradient []
        remove = repeatRemove sort__ (length sort__) (length sort__) []
        robots_new_working = robotsPosChild remove (length remove) (length remove) childs_pos []

        robots_old_pos = robotsPosOldTake remove (length remove) (length remove) 
        robots_new_pos = robotsPosNewTake remove (length remove) (length remove) 
        robots_random = takeRandomPosRobots board n m robots_old_pos robots_pos (length robots_pos) (length robots_pos) (robots_mask ++ robots_new_pos) r0 c0 rf cf

        in robots_random ++ robots_new_working
