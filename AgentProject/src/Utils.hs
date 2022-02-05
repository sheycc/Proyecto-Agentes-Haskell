module Utils where

    import Data.List
    import System.Random
    
    takePosCorralAvailables:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    takePosCorralAvailables board count r1 c1 r0 c0 rf cf       | count == 0 = []
                                                                | r1 == rf = []
                                                                | c1 == cf = []
                                                                | board!!r1!!c1 == 0  = [r1,c1] : takePosCorralAvailables board (count - 1) r1 (c1 + 1) r0 c0 rf cf
                                                                | otherwise = takePosCorralAvailables board count r1 (c1 + 1) r0 c0 rf cf

    takeAllCorralAvailables:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    takeAllCorralAvailables board r1 c1 r0 c0 rf cf | r1 == rf = []
                                                    | c1 == cf = []
                                                    | board!!r1!!c1 == 0  = [r1,c1] : takeAllCorralAvailables board r1 (c1 + 1) r0 c0 rf cf
                                                    | otherwise = takeAllCorralAvailables board r1 (c1 + 1) r0 c0 rf cf


    sortTuple:: [[Int]] -> [[Int]]
    sortTuple [] = []
    sortTuple xs = [m1,m2] : sortTuple (delete [m1,m2] xs)
                        where
                            m1 = minimum (takeTuple_0 xs (length xs) (length xs))
                            m2 = minimum (takeTuple_1 xs m1 (length xs) (length xs))
    
    takeTuple_0:: [[Int]] -> Int -> Int -> [Int]
    takeTuple_0 xs 0 p = []
    takeTuple_0 xs p0 p = head (xs!!(p - p0)) : takeTuple_0 xs (p0 - 1) p

    takeTuple_1:: [[Int]] -> Int -> Int -> Int -> [Int]
    takeTuple_1 xs r 0 p = []
    takeTuple_1 xs r p0 p = if head (xs!!(p - p0)) == r then xs!!(p - p0)!!1 : takeTuple_1 xs r (p0 - 1) p else takeTuple_1 xs r (p0 - 1) p

    takeValue:: [[Int]] -> Int -> Int -> Int -> Int -> Int
    takeValue robots r c p0 p   | p0 == 0 = -1
                                | (head (robots!!(p - p0)) == r) && ((robots!!(p - p0)!!1) == c) = robots!!(p - p0)!!2
                                | otherwise = takeValue robots r c (p0 - 1) p

    groupByRowChilds:: [[Int]] -> [[Int]]
    groupByRowChilds [] = []
    groupByRowChilds childs_in_corral = list : groupByRowChilds (childs_in_corral \\ delete_pos)
                                            where
                                                len = length childs_in_corral
                                                m = minimum (takeTuple_0 childs_in_corral len len)
                                                list = m : takeColumn m childs_in_corral len len
                                                delete_pos = takeRowColumn m childs_in_corral len len

    takeColumn:: Int -> [[Int]] -> Int -> Int -> [Int]
    takeColumn row childs p0 p  | p0 == 0 = []
                                | row == head (childs!!(p - p0)) = childs!!(p - p0)!!1 : takeColumn row childs (p0 - 1) p
                                | otherwise = takeColumn row childs (p0 - 1) p

    takeRowColumn:: Int -> [[Int]] -> Int -> Int -> [[Int]]
    takeRowColumn row childs p0 p   | p0 == 0 = []
                                    | row == head (childs!!(p - p0)) = childs!!(p - p0) : takeRowColumn row childs (p0 - 1) p
                                    | otherwise = takeRowColumn row childs (p0 - 1) p
                                

    takePos::[[Int]] -> Int -> Int -> Int -> [[Int]]
    takePos board n m value = takePos__ board value n n m m []

    takePos__:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
    takePos__ board value 0 n m0 m pos = pos
    takePos__ board value n0 n 0 m pos = takePos__ board value (n0 -1) n m m pos
    takePos__ board value n0 n m0 m pos = if board!!(n - n0)!!(m - m0) == value 
        then [[n - n0, m - m0]] ++ pos ++ takePos__ board value n0 n (m0 - 1) m pos else takePos__ board value n0 n (m0 - 1) m pos

    takePosChild::[[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    takePosChild board n m r0 c0 rf cf = takePosChilds__ board n n m m r0 c0 rf cf []

    takePosChilds__::[[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
    takePosChilds__ board 0 n m0 m r0 c0 rf cf pos = pos
    takePosChilds__ board n0 n 0 m r0 c0 rf cf pos = takePosChilds__ board (n0 - 1) n m m r0 c0 rf cf pos
    takePosChilds__ board n0 n m0 m r0 c0 rf cf pos = if (board!!(n - n0)!!(m - m0) == 1) && ((n - n0) <= r0 || (n - n0) >= rf || (m - m0) <= c0 || (m - m0) >= cf)
        then [[n - n0, m - m0]] ++ pos ++ takePosChilds__ board n0 n (m0 - 1) m r0 c0 rf cf pos else takePosChilds__ board n0 n (m0 - 1) m r0 c0 rf cf pos

    takePosRobots :: [[Int]] ->  Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    takePosRobots board n m r0 c0 rf cf = takePosRobots__ board n n m m r0 c0 rf cf
    
    takePosRobots__::[[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    takePosRobots__ board 0 n m0 m r0 c0 rf cf = []
    takePosRobots__ board n0 n 0 m r0 c0 rf cf = takePosRobots__ board (n0 - 1) n m m r0 c0 rf cf
    takePosRobots__ board n0 n m0 m r0 c0 rf cf = if (board!!(n - n0)!!(m - m0) == 4) && ((n - n0) <= r0 || (n - n0) >= rf || (m - m0) <= c0 || (m - m0) >= cf)
        then [n - n0, m - m0] : takePosRobots__ board n0 n (m0 - 1) m r0 c0 rf cf else takePosRobots__ board n0 n (m0 - 1) m r0 c0 rf cf


    print__ :: [[Int]] -> Int -> Int -> IO()
    print__ board n m = do
        printBoardWithColor board n n m
    
    printBoardWithColor :: [[Int]] -> Int -> Int -> Int -> IO()
    printBoardWithColor board 0 n m = return()
    printBoardWithColor board n0 n m = do
        let row = printRowWithColor board n0 n m m ""
        putStrLn row
        putStrLn ""
        printBoardWithColor board (n0 - 1) n m
    
    printRowWithColor::[[Int]] -> Int -> Int -> Int -> Int -> String -> String
    printRowWithColor board n0 n m0 m row   | m0 == 0 = row
                                            | board!!(n - n0)!!(m - m0) == -1 = head colors ++ "  " ++ row ++ printRowWithColor board n0 n (m0 - 1) m row
                                            | board!!(n - n0)!!(m - m0) == 0 = colors!!1 ++ "  " ++ row ++ printRowWithColor board n0 n (m0 - 1) m row
                                            | board!!(n - n0)!!(m - m0) == 1 = colors!!2 ++ "  " ++ row ++ printRowWithColor board n0 n (m0 - 1) m row
                                            | board!!(n - n0)!!(m - m0) == 2 = colors!!3 ++ "  " ++ row ++ printRowWithColor board n0 n (m0 - 1) m row
                                            | board!!(n - n0)!!(m - m0) == 3 = colors!!4 ++ "  " ++ row ++ printRowWithColor board n0 n (m0 - 1) m row
                                            | board!!(n - n0)!!(m - m0) == 4 = colors!!5 ++ "  " ++ row ++ printRowWithColor board n0 n (m0 - 1) m row
                                            | otherwise  = colors!!6 ++ "  " ++ row ++ printRowWithColor board n0 n (m0 - 1) m row
                                            where
                                                colors = ["\ESC[91m*", "\ESC[97m*", "\ESC[93m+", "\ESC[96m*", "\ESC[92mX", "\ESC[95mR", "\ESC[93mR"]

    generateNewBoard:: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
    generateNewBoard board n m childs_down_pos robots_up  = let
        board_without_robots = takeBoardWithoutRobots board n n m m 
        robots_up_sort = sortRobots robots_up
        in newBoardWithRobots board_without_robots n n m robots_up_sort (length robots_up) (length robots_up) childs_down_pos (length childs_down_pos) (length childs_down_pos)
    
    newBoardWithRobots:: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    newBoardWithRobots board n0 n m robots_up r0 r childs_add c0 c  | n0 == 0 = []
                                                                    | otherwise = let
                                                                        row = rowWithRobots__ board n0 n m m robots_up r0 r childs_add c0 c
                                                                        r1 = takeNextRow (n0 - 1) n robots_up r0 r
                                                                        c1 = takeNextRow (n0 - 1) n childs_add c0 c
                                                                        in row : newBoardWithRobots board (n0 - 1) n m robots_up r1 r childs_add c1 c

    rowWithRobots__:: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> [Int]
    rowWithRobots__ board n0 n m0 m robots_up r0 r childs_add c0 c  | m0 == 0 = []
                                                                    | r0 > 0 && (n - n0) == head (robots_up!!(r - r0)) && (m - m0) == robots_up!!(r -r0)!!1 = let
                                                                        value = robots_up!!(r - r0)!!2
                                                                        child = board!!(n - n0)!!(m - m0) == 1
                                                                        robot = if (value /= 0) && (value /= 3) then 5 else 4
                                                                        in  robot : rowWithRobots__ board n0 n (m0 - 1) m robots_up (r0 - 1) r childs_add c0 c
                                                                    | c0 > 0 && (n - n0) == head (childs_add!!(c - c0)) && (m - m0) == childs_add!!(c - c0)!!1 = 1 : rowWithRobots__ board n0 n (m0 - 1) m robots_up r0 r childs_add (c0 - 1) c
                                                                    | otherwise = board!!(n - n0)!!(m - m0) : rowWithRobots__ board n0 n (m0 - 1) m robots_up r0 r childs_add c0 c
 
    deleteRow:: Int -> [[Int]] -> Int -> Int -> [[Int]]                
    deleteRow r pos p0 p    | p0 == 0 = []
                            | head (pos!!(p - p0)) == r = deleteRow r pos (p0 - 1) p
                            | otherwise = (pos!!(p - p0)) : deleteRow r pos (p0 - 1) p

    sortRobots:: [[Int]] -> [[Int]]
    sortRobots [] = []
    sortRobots robots = [r, c, v] : sortRobots (delete [r, c, v] robots)
                        where
                            r = minimum (takeTuple_0 robots (length robots) (length robots))
                            c = minimum (takeTuple_1 robots r (length robots) (length robots))
                            v = takeValue robots r c (length robots) (length robots)
    
    takeBoardWithoutRobots::[[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
    takeBoardWithoutRobots board n0 n m0 m  | n0 == 0 = []
                                            | otherwise = let
                                                row = rowWithoutRobots board n0 n m0 m
                                                in row : takeBoardWithoutRobots board (n0 - 1) n m m
    
    rowWithoutRobots::[[Int]] -> Int -> Int -> Int -> Int -> [Int]
    rowWithoutRobots board n0 n m0 m    | m0 == 0 = []
                                        | (board!!(n - n0)!!(m - m0) /= 4) && (board!!(n - n0)!!(m - m0) /= 5) = board!!(n - n0)!!(m - m0) : rowWithoutRobots board n0 n (m0 - 1) m
                                        | otherwise = 0 : rowWithoutRobots board n0 n (m0 - 1) m
                                        
    takeSameDistance__::[[Int]] -> Int -> Int -> Int -> [Int]
    takeSameDistance__ runs distance 0 n = []
    takeSameDistance__ runs distance n0 n = if runs!!(n - n0)!!2 == distance then runs!!(n - n0) else takeSameDistance__ runs distance (n0 - 1) n

    takeSameDistance::[[Int]] -> Int -> Int -> Int -> [Int]
    takeSameDistance runs distance 0 n = []
    takeSameDistance runs distance n0 n = if head (runs!!(n - n0)) == distance then runs!!(n - n0) else takeSameDistance runs distance (n0 - 1) n

    containsAny::[[Int]] -> [Int] -> Int -> Int -> Bool
    containsAny list_ item 0 m = False
    containsAny list_ item m0 m =  (head (list_!!(m - m0)) == head item)
                                || (list_!!(m - m0)!!1 == item!!1)
                                || containsAny list_ item (m0 - 1) m
    
    containsAny__::[[[Int]]] -> [[Int]] -> Int -> Int -> Bool
    containsAny__ list_ item 0 m = False
    containsAny__ list_ item m0 m =   (head (list_!!(m - m0)) == head item)
                                    || (list_!!(m - m0)!!1 == item!!1)
                                    || ((list_!!(m - m0)!!2!!1 == item!!2!!1) && (list_!!(m - m0)!!2!!2 == item!!2!!2))
                                    || containsAny__ list_ item (m0 - 1) m

    containsAnyChild__::[[[Int]]] -> [[Int]] -> Int -> Int -> Bool
    containsAnyChild__ list_ item 0 m = False
    containsAnyChild__ list_ item m0 m = (head (list_!!(m - m0)) == head item)
                                        || containsAnyChild__ list_ item (m0 - 1) m


    takeTuple__:: [[[Int]]] -> Int -> Int -> Int -> [Int]
    takeTuple__ xs 0 p i = []
    takeTuple__ xs p0 p i = head (xs!!(p - p0)!!i) : takeTuple__ xs (p0 - 1) p i

    takeItemD:: [[[Int]]] -> Int -> Int -> Int -> [[Int]]
    takeItemD xs 0 p d = []
    takeItemD xs p0 p d = if head (xs!!(p - p0)!!2) == d then xs!!(p - p0) else takeItemD xs (p0 - 1) p d


    sortGradient:: [[[Int]]] -> [[[Int]]] -> [[[Int]]]
    sortGradient [] sort = sort
    sortGradient gradient sort = let
        d = minimum (takeTuple__ gradient (length gradient) (length gradient) 2)
        item = takeItemD gradient (length gradient) (length gradient) d
        in [item] ++ sort ++ sortGradient (delete item gradient) sort

    takeNextRow:: Int -> Int -> [[Int]] -> Int -> Int -> Int
    takeNextRow 0 n list_ l0 l = 0
    takeNextRow n0 n list_ 0 l = 0
    takeNextRow n0 n list_ l0 l = l1
        where
            l1 = if head (list_!!(l - l0)) >= (n - n0) then l0 else takeNextRow n0 n list_ (l0 - 1) l

    
    takePosToMove :: [[Int]] -> Int -> Int -> [[[Int]]] -> Int -> [[Int]]
    takePosToMove board n m position p  | p == 16 = []
                                        | available0 && available1 = posf : takePosToMove board n m position (p + 1)
                                        | otherwise = takePosToMove board n m position (p + 1)
                                        where
                                            pos0 = head (position!!p)
                                            posf = position!!p!!1
                                            available0 = (head pos0 >= 0) && (head pos0 < n) && (pos0!!1 >= 0) && (pos0!!1 < m) && ((board!!head pos0!!(pos0!!1) /= 3)  && (board!!head pos0!!(pos0!!1) /= 1))
                                            available1 = (head posf >= 0) && (head posf < n) && (posf!!1 >= 0) && (posf!!1 < m)  && (board!!head posf!!(posf!!1) /= -1) && (board!!head posf!!(posf!!1) /= 3) && (board!!head posf!!(posf!!1) /= 1)

    takePosToMoveCorral :: [[Int]] -> Int -> Int -> Int -> Int -> [[[Int]]] -> Int -> [[Int]]
    takePosToMoveCorral board r0 c0 rf cf position p    | p == 16 = []
                                                        | available0 && available1 = posf : takePosToMoveCorral board r0 c0 rf cf position (p + 1)
                                                        | otherwise = takePosToMoveCorral board r0 c0 rf cf position (p + 1)
                                                        where
                                                            pos0 = head (position!!p)
                                                            posf = position!!p!!1
                                                            available0 = (head pos0 > r0) && (head pos0 < rf) && (pos0!!1 > c0) && (pos0!!1 < cf) && (board!!head pos0!!(pos0!!1) == 0)
                                                            available1 = (head posf > r0) && (head posf < rf) && (posf!!1 > c0) && (posf!!1 < cf)  && (board!!head posf!!(posf!!1) == 0)



    posCorralSort :: [[Int]] -> [[Int]] -> Int -> Int -> [[[Int]]] -> [[[Int]]]
    posCorralSort [] pos p0 p add = sortSolve add
    posCorralSort pos_a pos 0 p add = posCorralSort (delete y pos_a) pos p p add
                                    where
                                        y = head pos_a
                                         
    posCorralSort pos_a pos p0 p add = posCorralSort pos_a pos (p0 - 1) p ([[dif, r, c], x] : add)
                                        where
                                            x = pos!!(p - p0)
                                            r_ = head x
                                            c_ = x!!1
                                            y = head pos_a
                                            r = head y
                                            c = y!!1
                                            dif = abs (r - r_) + abs (c - c_)
    
    takeTupleRest :: [[[Int]]] -> Int -> Int -> Int -> [[Int]]
    takeTupleRest xs dif p0 p   | p0 == 0 = []
                                | dif == d = [[head t!!1, head t!!2], t!!1]
                                | otherwise = takeTupleRest xs dif (p0 - 1) p
                                where
                                    t = xs!!(p - p0)
                                    d = head (head t)

    takeTuple_00 :: [[[Int]]] -> Int -> Int -> [Int]
    takeTuple_00 xs 0 p = []
    takeTuple_00 xs p0 p = d : takeTuple_00 xs (p0 - 1) p
                            where
                                t = xs!!(p - p0)
                                d = head (head t)

    sortSolve :: [[[Int]]] -> [[[Int]]]
    sortSolve [] = []
    sortSolve xs = [pos0, posf] : sortSolve (delete [[dif, head pos0, pos0!!1], posf] xs)
                    where
                        dif = minimum (takeTuple_00 xs (length xs) (length xs))
                        pos0:posf:_ = takeTupleRest xs dif (length xs) (length xs)

    takeSequenceCorral :: Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> [[Int]] -> [[[Int]]]
    takeSequenceCorral r1 c1 r0 c0 rf cf childs count sum_ in_order new_pos | count == 0 = [in_order, new_pos]
                                                                            | (r1 + sum_ >= rf) || (r1 - sum_) <= r0 || (c1 + sum_ >= cf) || (c1 + sum_ <= c0) = [in_order, new_pos]
                                                                            | otherwise = let
                                                                                dr = [   0,    0, sum_, sum_, sum_, -sum_, -sum_, -sum_]
                                                                                dc = [sum_, -sum_, sum_, -sum_,   0, -sum_,  sum_,    0]

                                                                                possible1 = count > 0
                                                                                pos1_ = [[r1 + head dr, c1 + head dc] | possible1 && not (containsAny childs [r1 + head dr, c1 + head dc] (length childs) (length childs))]  
                                                                                in_order1 = [[r1 + head dr, c1 + head dc] | possible1 && null pos1_]

                                                                                possible2 = (count - length pos1_) > 0
                                                                                pos2_ = [[r1 + dr!!1, c1 + dc!!1] | possible2 && not (containsAny childs [r1 + dr!!1, c1 + dc!!1] (length childs) (length childs))]
                                                                                in_order2 = [[r1 + dr!!1, c1 + dc!!1] | possible2 && null pos2_]

                                                                                possible3 = (count - length pos1_ - length pos2_) > 0
                                                                                pos3_ = [[r1 + dr!!2, c1 + dc!!2] | possible3 && not (containsAny childs [r1 + dr!!2, c1 + dc!!2] (length childs) (length childs))]
                                                                                in_order3 = [[r1 + dr!!2, c1 + dc!!2] | possible3 && null pos3_]

                                                                                possible4 = (count - length pos1_ - length pos2_ - length pos3_) > 0
                                                                                pos4_ = [[r1 + dr!!3, c1 + dc!!3] | possible4 && not (containsAny childs [r1 + dr!!3, c1 + dc!!3] (length childs) (length childs))]
                                                                                in_order4 = [[r1 + dr!!3, c1 + dc!!3] | possible4 && null pos4_]
                                                               
                                                                                possible5 = (count - length pos1_ - length pos2_ - length pos3_ - length pos4_) > 0
                                                                                pos5_ = [[r1 + dr!!4, c1 + dc!!4] | possible5 && not (containsAny childs [r1 + dr!!4, c1 + dc!!4] (length childs) (length childs))]
                                                                                in_order5 = [[r1 + dr!!4, c1 + dc!!4] | possible5 && null pos5_]
                                                               
                                                                                possible6 = (count - length pos1_ - length pos2_ - length pos3_ - length pos4_ - length pos5_ > 0)
                                                                                pos6_ = [[r1 + dr!!5, c1 + dc!!5] | possible6 && not (containsAny childs [r1 + dr!!5, c1 + dc!!5] (length childs) (length childs))]
                                                                                in_order6 = [[r1 + dr!!5, c1 + dc!!5] | possible6 && null pos6_]

                                                                                possible7 = (count - length pos1_ - length pos2_ - length pos3_ - length pos4_ - length pos5_ - length pos6_) > 0
                                                                                pos7_ = [[r1 + dr!!6, c1 + dc!!6] | possible7 && not (containsAny childs [r1 + dr!!6, c1 + dc!!6] (length childs) (length childs))]
                                                                                in_order7 = [[r1 + dr!!6, c1 + dc!!6] | possible7 && null pos7_]
                                                             
                                                                                possible8 = (count - length pos1_ - length pos2_ - length pos3_ - length pos4_ - length pos5_ - length pos6_ - length pos7_) > 0
                                                                                pos8_ = [[r1 + dr!!7, c1 + dc!!7] | possible8 && not (containsAny childs [r1 + dr!!7, c1 + dc!!7] (length childs) (length childs))]
                                                                                in_order8 = [[r1 + dr!!7, c1 + dc!!7] | possible8 && null pos8_]
                                                                
                                                                                in takeSequenceCorral r1 c1 r0 c0 rf cf childs (count - length pos1_ - length pos2_ - length pos3_ - length pos4_ - length pos5_ - length pos6_ - length pos7_ - length pos8_) (sum_ + 1) (in_order1 ++ in_order2 ++ in_order3 ++ in_order4 ++ in_order5 ++ in_order6 ++ in_order7 ++ in_order8 ++ in_order) (pos1_ ++ pos2_ ++ pos3_ ++ pos4_ ++ pos5_ ++ pos6_ ++ pos7_ ++ pos8_ ++ new_pos)

    takeChildsInCorral:: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    takeChildsInCorral childs r0 c0 rf cf p0 p  | p0 == 0 = []
                                                | (head (childs!!(p - p0)) > r0) && (head (childs!!(p - p0)) < rf) && ((childs!!(p - p0)!!1) > c0) && ((childs!!(p - p0)!!1) < cf) = (childs!!(p - p0)) : takeChildsInCorral childs r0 c0 rf cf (p0 - 1) p
                                                | otherwise = takeChildsInCorral childs r0 c0 rf cf (p0 - 1) p

    takePosChildInCorral :: [[Int]] -> Int -> Int -> Int -> Int-> Int -> [[Int]]
    takePosChildInCorral board n0 n m0 m1 m | n0 == n = []
                                            | m0 == m = takePosChildInCorral board (n0 + 1) n m1 m1 m
                                            | board!!n0!!m0 == 1 = [n0, m0] : takePosChildInCorral board n0 n (m0 + 1) m1 m
                                            | otherwise = takePosChildInCorral board n0 n (m0 + 1) m1 m

    takeNewPosRobot :: [[Int]] -> Int -> Int -> Int -> [Int]
    takeNewPosRobot distances d 0 p = []
    takeNewPosRobot distances d p0 p = if d == head (distances!!(p - p0)) then [distances!!(p - p0)!!1, distances!!(p - p0)!!2] 
                                                                            else takeNewPosRobot distances d (p0 - 1) p
