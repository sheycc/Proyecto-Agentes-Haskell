module DFS where
    import Data.List (find, nub)
    import Data.Maybe (listToMaybe, mapMaybe, isNothing, isJust)
    import Debug.Trace (trace)
    import Utils (containsAny)
    
    data SearchProb a = 
        SearchProb { start :: a,
                     expand :: a -> [a],
                     isDone :: a -> Bool
                    }
    
    find_ :: [[Int]] -> Int -> Int -> [Int] -> [Int] -> SearchProb [Int]
    find_ board n m pos0 posf = SearchProb start expand isDone where
        start = pos0
        expand = \[x,y] ->      [[x + 1, y] | (x < (n - 1)) && (board!!(x + 1)!!y /= -1) && (board!!(x + 1)!!y /= 3) && (board!!(x + 1)!!y /= 1)] ++ 
                                [[x, y + 1] | (y < (m - 1)) && (board!!x!!(y + 1) /= -1) && (board!!x!!(y + 1) /= 3) && (board!!x!!(y + 1) /= 1)] ++ 
                                [[x + 1, y + 1] | (x < (n - 1)) && (y < (m - 1)) && (board!!(x + 1)!!(y + 1) /= -1) && (board!!(x + 1)!!(y + 1) /= 3) && (board!!(x + 1)!!(y + 1) /= 1)] ++ 
                                [[x - 1, y] | (x > 0) && (board!!(x - 1)!!y /= -1) && (board!!(x - 1)!!y /= 3) && (board!!(x - 1)!!y /= 1)] ++ 
                                [[x, y - 1] | (y > 0) && (board!!x!!(y - 1) /= -1) && (board!!x!!(y - 1) /= 3) && (board!!x!!(y - 1) /= 1)] ++ 
                                [[x - 1 , y - 1] | (x > 0) && (y > 0) && (board!!(x - 1)!!(y - 1) /= -1) && (board!!(x - 1)!!(y - 1) /= 3) && (board!!(x - 1)!!(y - 1) /= 1)] ++ 
                                [[x - 1 , y + 1] | (x > 0) && (y < (m - 1)) && (board!!(x - 1)!!(y + 1) /= -1) && (board!!(x - 1)!!(y + 1) /= 3) && (board!!(x - 1)!!(y + 1) /= 1)] ++ 
                                [[x + 1 , y - 1] | (x < (n - 1)) && (y > 0) && (board!!(x + 1)!!(y - 1) /= -1) && (board!!(x + 1)!!(y - 1) /= 3) && (board!!(x + 1)!!(y - 1) /= 1)]
        isDone = ( == posf)
    
    type SearchAlgo a = SearchProb a -> Maybe a

    dfs__ :: Eq a => SearchAlgo a
    dfs__ (SearchProb start expand isDone) = loop [start] where
        loop xs | any isDone xs = find isDone xs
                | otherwise = loop (nub $ concatMap expand xs)


    nextPosition__:: [[Int]] -> Int -> Int -> [Int] -> [Int] -> [[Int]] -> [Int]
    nextPosition__ board n m pos0 posf robots_mask = let
        r = head pos0
        c = pos0!!1
        available = not (containsAny robots_mask pos0 (length robots_mask) (length robots_mask))
                        && ((board!!r!!c) == 0 || (board!!r!!c) == 1)
        dfs = if available then dfs__ (find_ board n m pos0 posf) else Nothing
        in if isJust dfs then pos0 else []