module Background where

    import System.IO
    import Data.List
    import Data.Maybe
    import System.Random
    import Utils
    import R_C_in_corral
    

    -- board complete
    boardComplete:: Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> [[Int]]
    boardComplete n m r0 c0 rf cf board c = let
        possible_pos_robots = takeEmptyBoxRobots board n n m m r0 c0 rf cf
        g = mkStdGen c
        robots_positions = posGen possible_pos_robots (div (n * m) 20) g
        in_order_robots = sortTuple robots_positions
        in boardComplete__ board n n m in_order_robots (length in_order_robots) (length in_order_robots) [] 

    boardComplete__:: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
    boardComplete__ board 0 n m robots 0 l matrix  = matrix
    boardComplete__ board n0 n m robots l0 l matrix  = let
        row = rowWithRobots n0 n m m board robots l0 l []
        l1 = takeNextRow (n0 - 1) n robots l0 l
        in [row] ++ matrix ++ boardComplete__ board (n0 - 1) n m robots l1 l matrix

    rowWithRobots:: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]] -> Int -> Int -> [Int] -> [Int]
    rowWithRobots n0 n 0 m board robots l0 l row = row
    rowWithRobots n0 n m0 m board robots 0 l row = [board!!(n - n0)!!(m - m0)] ++ row ++ rowWithRobots n0 n (m0 - 1) m board robots 0 l row
    rowWithRobots n0 n m0 m board robots l0 l row = let
        value = if head (robots!!(l - l0)) == (n - n0) && robots!!(l - l0)!!1 == (m - m0) then 4 else board!!(n - n0)!!(m - m0)
        in if value == 4 then [value] ++ row ++ rowWithRobots n0 n (m0 - 1) m board robots (l0 - 1) l row 
            else [value] ++ row ++ rowWithRobots n0 n (m0 - 1) m board robots l0 l row


    -- board with obstacles, dirt, childs and corral
    boardGenerateWithObstacles:: Int -> Int -> Int -> [[Int]] -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    boardGenerateWithObstacles n0 n m board_dirt r0 c0 rf cf c = let
        possible_pos_obstacles = takeAvailableBox board_dirt n n m m r0 c0 rf cf
        g = mkStdGen c
        obstacles_positions = posGen possible_pos_obstacles (div (n * m) 10) g
        in_order_obstacles = sortTuple obstacles_positions
        in boardWithObstacles__ board_dirt n n m in_order_obstacles (length obstacles_positions) (length obstacles_positions) [] 

    boardWithObstacles__:: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
    boardWithObstacles__ board 0 n m obstacles 0 l matrix  = matrix
    boardWithObstacles__ board n0 n m obstacles l0 l matrix  = let
        row = rowWithObstacles n0 n m m board obstacles l0 l []
        l1 = takeNextRow (n0 - 1) n obstacles l0 l
        in [row] ++ matrix ++ boardWithObstacles__ board (n0 - 1) n m obstacles l1 l matrix

    rowWithObstacles:: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]] -> Int -> Int -> [Int] -> [Int]
    rowWithObstacles n0 n 0 m board obstacles l0 l row = row
    rowWithObstacles n0 n m0 m board obstacles 0 l row = [board!!(n - n0)!!(m - m0)] ++ row ++ rowWithObstacles n0 n (m0 - 1) m board obstacles 0 l row
    rowWithObstacles n0 n m0 m board obstacles l0 l row = let
        value = if head (obstacles!!(l - l0)) == (n - n0) && obstacles!!(l - l0)!!1 == (m - m0) then 3 else board!!(n - n0)!!(m - m0)
        in if value == 3 then [value] ++ row ++ rowWithObstacles n0 n (m0 - 1) m board obstacles (l0 - 1) l row 
            else [value] ++ row ++ rowWithObstacles n0 n (m0 - 1) m board obstacles l0 l row


    takeAvailableBox:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    takeAvailableBox board 0 n m0 m r0 c0 rf cf = []
    takeAvailableBox board n0 n m0 m r0 c0 rf cf = let
        new_n0 = if m0 == 1 then n0 - 1 else n0
        new_m0 = if m0 == 1 then m else m0 - 1
        corral_out = ((n - n0) < r0 || (n - n0) > rf) && ((m - m0) < c0 || (m - m0) > cf)
        available_box = corral_out && board!!(n - n0)!!(m - m0) == 0
        in if available_box then [n - n0, m - m0] : takeAvailableBox board new_n0 n new_m0 m r0 c0 rf cf else takeAvailableBox board new_n0 n new_m0 m r0 c0 rf cf


    -- board with dirt, childs and corral
    boardGenerateWithDirt:: Int -> Int -> Int -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> [[Int]]
    boardGenerateWithDirt 0 n m board_childs r0 c0 rf cf board_dirt c = board_dirt
    boardGenerateWithDirt n0 n m board_childs r0 c0 rf cf board_dirt c = let
        g = mkStdGen c
        row = adyacentRowsWithDirt n0 n m m board_childs r0 c0 rf cf [] g
        in [row] ++ board_dirt ++ boardGenerateWithDirt (n0 - 1) n m board_childs r0 c0 rf cf board_dirt (c + c)

    adyacentRowsWithDirt:: Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> Int -> Int -> [Int] -> StdGen -> [Int]
    adyacentRowsWithDirt n0 n 0 m board_childs r0 c0 rf cf row g = row
    adyacentRowsWithDirt n0 n m0 m board_childs r0 c0 rf cf row g = let
        childs_left = ((m - m0) - 1) >= 0 && board_childs!!(n - n0)!!((m - m0) - 1) == 1
        childs_right = ((m - m0) + 1) < m && board_childs!!(n - n0)!!((m - m0) + 1) == 1
        childs_up = ((n - n0) - 1) >= 0 && board_childs!!((n - n0) - 1)!!(m - m0) == 1
        childs_down = ((n - n0) + 1) < n && board_childs!!((n - n0) + 1)!!(m - m0) == 1
        corral_out = (n - n0) <= r0 || (n - n0) >= rf || (m - m0) <= c0 || (m - m0) >= cf
        empty_box = board_childs!!(n - n0)!!(m - m0) == 0
        (possible_dirt, f) = randomR (True, False) g
        value = if empty_box && corral_out && possible_dirt && (childs_left || childs_right || childs_up || childs_down) then 2 else board_childs!!(n - n0)!!(m - m0)
        in [value] ++ row ++ adyacentRowsWithDirt n0 n (m0 - 1) m board_childs r0 c0 rf cf row f



    --board with childs and corral
    boardGenerateWithChilds :: Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    boardGenerateWithChilds n m r0 c0 rf cf board_corral childs_count c = let
        possible_pos_childs = takeEmptyBox board_corral n n m m r0 c0 rf cf
        g = mkStdGen c
        childs_pos = posGen possible_pos_childs childs_count g
        childs_corral = takeChildsInCorral childs_pos r0 c0 rf cf (length childs_pos) (length childs_pos)
        childs_in_corral_new = if not (null childs_corral) then takePosCorralAvailables board_corral (length childs_corral) (r0 + 1) (c0 + 1) r0 c0 rf cf
                                                            else []
        in_order_childs = sortTuple ((childs_pos \\ childs_corral) ++ childs_in_corral_new)
        in boardWithChilds board_corral n n m in_order_childs (length in_order_childs) (length in_order_childs) [] 

    boardWithChilds:: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
    boardWithChilds board 0 n m childs 0 l matrix  = matrix
    boardWithChilds board n0 n m childs l0 l matrix  = let
        row = rowWithChilds n0 n m m board childs l0 l []
        l1 = takeNextRow (n0 - 1) n childs l0 l
        in [row] ++ matrix ++ boardWithChilds board (n0 - 1) n m childs l1 l matrix

    rowWithChilds:: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]] -> Int -> Int -> [Int] -> [Int]
    rowWithChilds n0 n 0 m board childs l0 l row = row
    rowWithChilds n0 n m0 m board childs 0 l row = [board!!(n - n0)!!(m - m0)] ++ row ++ rowWithChilds n0 n (m0 - 1) m board childs 0 l row
    rowWithChilds n0 n m0 m board childs l0 l row = let
        value = if head (childs!!(l - l0)) == (n - n0) && childs!!(l - l0)!!1 == (m - m0) then 1 else board!!(n - n0)!!(m - m0)
        in if value == 1 then [value] ++ row ++ rowWithChilds n0 n (m0 - 1) m board childs (l0 - 1) l row 
            else [value] ++ row ++ rowWithChilds n0 n (m0 - 1) m board childs l0 l row


    --board with corral
    boardGenerateCorral:: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
    boardGenerateCorral 0 n m r0 c0 rf cf matrix = matrix
    boardGenerateCorral n0 n m r0 c0 rf cf matrix = rowGenCorral m m n0 n r0 c0 rf cf [] : boardGenerateCorral (n0 - 1) n m r0 c0 rf cf matrix 

    rowGenCorral :: Int -> Int -> Int -> Int -> Int ->Int -> Int -> Int -> [Int] -> [Int]
    rowGenCorral 0 m n0 n r0 c0 rf cf row = row
    rowGenCorral m0 m n0 n r0 c0 rf cf row  | range_row || range_column = [-1] ++ row ++ rowGenCorral (m0 - 1) m n0 n r0 c0 rf cf row
                                        | otherwise = [0] ++ row ++ rowGenCorral (m0 - 1) m n0 n r0 c0 rf cf row
                                        where 
                                            range_row = (((n - n0) == r0) || ((n - n0) == rf)) && (c0 < (m - m0) && cf > (m - m0)) 
                                            range_column = ((c0 == (m - m0)) || (cf == (m - m0))) && ((n - n0) > r0 && (n - n0) < rf)

    --utils
    takeEmptyBox:: [[Int]] -> Int -> Int -> Int -> Int ->Int -> Int -> Int -> Int ->  [[Int]]
    takeEmptyBox board 0 n m0 m r0 c0 rf cf = []
    takeEmptyBox board n0 n m0 m r0 c0 rf cf = let
        new_n0 = if m0 == 1 then n0 - 1 else n0
        new_m0 = if m0 == 1 then m else m0 - 1
        box_available = (board!!(n - n0)!!(m - m0) == 0) && ([n - n0, m - m0] /= [r0,c0]) && ([n - n0, m - m0] /= [r0,cf]) && ([n - n0, m - m0] /= [rf,c0]) && ([n - n0, m - m0] /= [rf,cf])

        in if box_available then [n - n0, m - m0] : takeEmptyBox board new_n0 n new_m0 m r0 c0 rf cf else takeEmptyBox board new_n0 n new_m0 m r0 c0 rf cf

    takeEmptyBoxRobots:: [[Int]] -> Int -> Int -> Int -> Int ->Int -> Int -> Int -> Int ->  [[Int]]
    takeEmptyBoxRobots board 0 n m0 m r0 c0 rf cf = []
    takeEmptyBoxRobots board n0 n m0 m r0 c0 rf cf = let
        new_n0 = if m0 == 1 then n0 - 1 else n0
        new_m0 = if m0 == 1 then m else m0 - 1
        box_available = (board!!(n - n0)!!(m - m0) == 0) && ((n - n0) < r0 || (n - n0) > rf || (m - m0) < c0 || (m - m0) > cf)
        in if box_available then [n - n0, m - m0] : takeEmptyBoxRobots board new_n0 n new_m0 m r0 c0 rf cf else takeEmptyBoxRobots board new_n0 n new_m0 m r0 c0 rf cf


    posGen :: [[Int]] -> Int -> StdGen -> [[Int]]
    posGen [] c g = []
    posGen positions 0 g = []
    posGen positions c g = let
        (p, f) = randomR (0, length positions - 1) g
        pos = positions!!p
        in pos : posGen (delete pos positions) (c - 1) f

    countRandom :: Int -> IO Int
    countRandom n = do randomRIO(2, n :: Int)

    boardChildsMove :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    boardChildsMove board 0 n m childs i0 i obst j0 j = []
    boardChildsMove board n0 n m childs i0 i obst j0 j = let
        row = rowChildsMove board n0 n m m childs i0 i obst j0 j
        i1 = takeNextRow (n0 - 1) n childs i0 i
        j1 = takeNextRow (n0 - 1) n obst j0 j
        in row : boardChildsMove board (n0 - 1) n m childs i1 i obst j1 j
    
    rowChildsMove :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> [Int]
    rowChildsMove board n0 n 0 m childs i0 i obsts j0 j = []                                          
    rowChildsMove board n0 n m0 m childs i0 i obsts j0 j = let
        pos | (i0 > 0) && ([n - n0, m - m0] == childs!!(i - i0)) = 1
            | (j0 > 0) && ([n - n0, m - m0] == obsts!!(j - j0)) = 3
            | (board!!(n - n0)!!(m - m0) == 1) || (board!!(n - n0)!!(m - m0) == 3) = 0
            | otherwise = board!!(n - n0)!!(m - m0) 
        i1 = if pos == 1 then i0 - 1 else i0
        j1 = if pos == 3 then j0 - 1 else j0
        in pos : rowChildsMove board n0 n (m0 - 1) m childs i1 i obsts j1 j                                   

    takePosObstacles :: [[Int]] -> Int -> Int -> Int -> Int -> [[[Int]]] -> [[Int]]
    takePosObstacles board 0 n m0 m childs_dir = []
    takePosObstacles board n0 n 0 m childs_dir = takePosObstacles board (n0 - 1) n m m childs_dir
    takePosObstacles board n0 n m0 m childs_dir = let
        child = childIn childs_dir [n - n0, m - m0] (length childs_dir) (length childs_dir)
        c = head child
        d = child!!1
        pos | (board!!(n - n0)!!(m - m0)) == 3 && null child = [n - n0, m - m0]
            | (board!!(n - n0)!!(m - m0)) == 3 && not (null child) = takePosObs board n m (head c + head d) (c!!1 + d!!1) d
            | otherwise = []
        in if null pos then takePosObstacles board n0 n (m0 - 1) m childs_dir
                        else pos : takePosObstacles board n0 n (m0 - 1) m childs_dir
    
    childIn :: [[[Int]]] -> [Int] -> Int -> Int -> [[Int]]
    childIn child_dir pos p0 p  | p0 == 0 = []
                                | head (child_dir!!(p - p0)) == pos = child_dir!!(p - p0)
                                | otherwise = childIn child_dir pos (p0 - 1) p

    takeMoveChilds :: [[[Int]]] -> Int -> Int -> [[Int]]
    takeMoveChilds childs_dir_new p0 p  | p0 == 0 = []
                                        | otherwise = head (childs_dir_new!!(p - p0)) : takeMoveChilds childs_dir_new (p0 - 1) p

    takeMoveChildsDir :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> StdGen -> [[Int]] -> [[[Int]]]
    takeMoveChildsDir board n m r0 c0 rf cf childs 0 p g mask = []
    takeMoveChildsDir board n m r0 c0 rf cf childs p0 p g mask = let
        r_child = head (childs!!(p - p0))
        c_child = childs!!(p - p0)!!1
        run1 = [[r_child - 1, c_child] | childPossibleMove board n m r0 c0 rf cf (r_child - 1) c_child [-1,0] mask]
        dir1 = [[-1, 0] | not (null run1)]
        run2 = [[r_child - 1, c_child - 1] | childPossibleMove board n m r0 c0 rf cf (r_child - 1) (c_child - 1) [-1,-1] mask]
        dir2 = [[-1,-1] | not (null run2)]
        run3 = [[r_child - 1, c_child + 1] | childPossibleMove board n m r0 c0 rf cf (r_child - 1) (c_child + 1) [-1, 1] mask]
        dir3 = [[-1, 1] | not (null run3)]
        run4 = [[r_child + 1, c_child] | childPossibleMove board n m r0 c0 rf cf (r_child + 1) c_child [1,0] mask]
        dir4 = [[1, 0] | not (null run4)]
        run5 = [[r_child + 1, c_child - 1] | childPossibleMove board n m r0 c0 rf cf (r_child + 1) (c_child - 1) [1,-1] mask]
        dir5 = [[1, -1] | not (null run5)]
        run6 = [[r_child + 1, c_child + 1] | childPossibleMove board n m r0 c0 rf cf (r_child + 1) (c_child + 1) [1,1] mask]
        dir6 = [[1, 1] | not (null run6)]
        run7 = [[r_child, c_child - 1] | childPossibleMove board n m r0 c0 rf cf r_child (c_child - 1) [0,-1] mask]
        dir7 = [[0,-1] | not (null run7)]
        run8 = [[r_child, c_child + 1] | childPossibleMove board n m r0 c0 rf cf r_child (c_child + 1) [0, 1] mask]
        dir8 = [[0,1] | not (null run8)]

        runs = run1 ++ run2 ++ run3 ++ run4 ++ run5 ++ run6 ++ run7 ++ run8
        dirs = dir1 ++ dir2 ++ dir3 ++ dir4 ++ dir5 ++ dir6 ++ dir7 ++ dir8

        (i, f) = randomR (0, length runs - 1) g
        run_i = if null runs then [r_child, c_child] else runs!!i
        dir_i = if null dirs then [0,0] else dirs!!i

        in [run_i, dir_i] : takeMoveChildsDir board n m r0 c0 rf cf childs (p0 - 1) p f (run_i : mask)

    childPossibleMove :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [[Int]] -> Bool
    childPossibleMove board n m r0 c0 rf cf row col dir mask    | (row < 0) || (row >= n) || (col < 0) || (col >= m) = False 
                                                                | containsAny mask [row,col] (length mask)  (length mask) = False
                                                                | ([row,col] == [r0,c0]) || ([row,col] == [r0,cf]) || ([row,col] == [rf,c0]) || ([row,col] == [rf,cf]) = False
                                                                | board!!row!!col == 0 = True
                                                                | board!!row!!col == 3 = childPossibleMove board n m r0 c0 rf cf (row + head dir) (col + dir!!1) dir mask
                                                                | otherwise = False

    takePosObs :: [[Int]] -> Int -> Int -> Int -> Int -> [Int] -> [Int]
    takePosObs board n m row col dir    | (row < 0) || (row >= n) || (col < 0) || (col >= m) = []
                                        | board!!row!!col == 0 = [row, col]
                                        | board!!row!!col == 3 = takePosObs board n m (row + head dir) (col + dir!!1) dir
                                        | otherwise = []

    --hacer esto
    takeBoxes3x3 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> StdGen -> [[Int]]
    takeBoxes3x3 board n0 n m0 m r0 c0 rf cf g  | n0 >= n = []
                                                | m0 >= m = takeBoxes3x3 board (n0 + 3) n 1 m r0 c0 rf cf g
                                                | (n0 > r0) && (n0 < rf) && (m0 > c0) && (m0 < cf) = takeBoxes3x3 board n0 n (cf + 1) m r0 c0 rf cf g
                                                | otherwise = let
                                                    runs = [[n0, m0], [n0 - 1, m0], [n0 - 1, m0 + 1], [n0 - 1, m0 - 1], [n0 + 1, m0], [n0 + 1, m0 + 1], [n0 + 1, m0 - 1], [n0, m0 + 1], [n0, m0 - 1]]
                                                    c_count = takeChildsCount board n m r0 c0 rf cf runs 0
                                                    (d_count, f)    | c_count == 0 = (0,g)
                                                                    | c_count == 1 = randomR (0,1) g
                                                                    | c_count == 2 = randomR (0,3) g
                                                                    | otherwise = randomR (0,6) g
                                                    new_pos = takeNewPosDirt board n m runs 0 d_count r0 c0 rf cf
                                                    in new_pos ++ takeBoxes3x3 board n0 n (m0 + 3) m r0 c0 rf cf f

    takeNewPosDirt :: [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    takeNewPosDirt board n m runs 9 d_count r0 c0 rf cf = []
    takeNewPosDirt board n m runs i 0 r0 c0 rf cf = []
    takeNewPosDirt board n m runs i d_count r0 c0 rf cf = let
        pos | (head (runs!!i) < 0) || (head (runs!!i) >= n) || (runs!!i!!1 < 0) || (runs!!i!!1 >= m) = []
            | (head (runs!!i) > r0) && (head (runs!!i) < rf) && (runs!!i!!1 > c0) && (runs!!i!!1 < cf) = []
            | board!!head (runs!!i)!!(runs!!i!!1) == 0 = runs!!i
            | otherwise = []
        in if null pos then takeNewPosDirt board n m runs (i + 1) d_count r0 c0 rf cf
                        else pos : takeNewPosDirt board n m runs (i + 1) (d_count - 1) r0 c0 rf cf  

    takeChildsCount :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int 
    takeChildsCount board n m r0 c0 rf cf runs 9 = 0
    takeChildsCount board n m r0 c0 rf cf runs i = let
        add | (head (runs!!i) < 0) || (head (runs!!i) >= n) || (runs!!i!!1 < 0) || (runs!!i!!1 >= m) = 0
            | (head (runs!!i) > r0) && (head (runs!!i) < rf) && (runs!!i!!1 > c0) && (runs!!i!!1 < cf) = 0
            | board!!head (runs!!i)!!(runs!!i!!1) == 1 = 1
            | otherwise = 0
        in add + takeChildsCount board n m r0 c0 rf cf runs (i + 1) 

    boardWithDirt :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    boardWithDirt board 0 n m dirt i0 i = []
    boardWithDirt board n0 n m dirt i0 i = let
        row = rowWithDirt board n0 n m m dirt i0 i
        i1 = takeNextRow (n0 - 1) n dirt i0 i
        in row : boardWithDirt board (n0 - 1) n m dirt i1 i
    
    rowWithDirt :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [Int]
    rowWithDirt board n0 n 0 m dirt i0 i = []                                          
    rowWithDirt board n0 n m0 m dirt i0 i = let
        (pos , take)    | (i0 > 0) && ([n - n0, m - m0] == dirt!!(i - i0)) = (2, True)
                        | otherwise = (board!!(n - n0)!!(m - m0), False) 
        i1 = if take then i0 - 1 else i0
        in pos : rowWithDirt board n0 n (m0 - 1) m dirt i1 i                                 
