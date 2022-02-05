module Main where

import Background
import Utils
import BDI
import System.Random
import Data.List

main :: IO ()
main = do
  putStrLn "\ESC[93m> n:"
  ns <- getLine
  putStrLn "\ESC[93m> m:"
  ms <- getLine
  putStrLn "\ESC[93m> t:"
  ts <- getLine
  putStrLn ""
  do
    --generate_board
    let n = read ns :: Int
    let m = read ms :: Int
    let t = read ts :: Int

    let childs = m
    let r0 : c0: rf : cf : _ = [n - 2, -1, n, m]

    c <- randomRIO (789, 765489 :: Int)
    let board_shape = boardGenerateCorral n n m r0 c0 rf cf []
    let board_childs = boardGenerateWithChilds n m r0 c0 rf cf board_shape childs c
    let board_dirt = boardGenerateWithDirt n n m board_childs r0 c0 rf cf [] (c + 5)
    let board_obstacles = boardGenerateWithObstacles n n m board_dirt r0 c0 rf cf (c * 7)
    let board_complete_with_robots = boardComplete n m r0 c0 rf cf board_obstacles (c - 2)
    print__ board_complete_with_robots n m
    
    --believes-desires-intentions
    do
      putStrLn "\ESC[90m Descripcion: "
      putStrLn ""
      putStrLn "\ESC[91m Las casillas del corral se representan en rojo (*)"
      putStrLn "\ESC[97m Las casillas vacias se representan en blanco (*)"
      putStrLn "\ESC[93m Los ninnos se representan en amarillo (+)"
      putStrLn "\ESC[96m La suciedad se representa en azul celeste (*)"
      putStrLn "\ESC[92m Los obstaculos se representan en verde (X)"
      putStrLn "\ESC[95m Los robots de casa se representan en morado (R)"
      putStrLn "\ESC[93m Los robots con ninnos se representan en amarillo (R)"
      putStrLn ""

      print__ board_complete_with_robots n m
      agentStart 10 t t board_complete_with_robots r0 c0 rf cf childs n m []

  return()


agentStart:: Int -> Int -> Int -> [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> IO()

agentStart 0 t0 t board r0 c0 rf cf childs n m robots_up = return()

agentStart count 0 t board r0 c0 rf cf childs n m robots_up = do
  
  putStrLn "\ESC[94m Nuevo turno"
  putStrLn "\ESC[94m /////////////////////////////////////////////////////////////////////////////////////////////////////////////"

  let home_status__ = homeClean board n m
  let board_by_environment:new_robots_up: _ = if home_status__ then [board, robots_up] else bdiStart board n m r0 c0 rf cf robots_up
  let home_status | home_status__ = "\ESC[97m La casa se encuentra limpia"
                  | otherwise = "\ESC[96m La casa se encuentra sucia"
  
  putStrLn home_status
  let agents_str  | home_status__ = "\ESC[95m Agentes esperando . . ."
                  | otherwise = "\ESC[95m Acciones de los agentes . . ."

  putStrLn "\ESC[95m ------------------------------------------------------------------------------------------------------------"
  putStrLn agents_str
  print__ board_by_environment n m
  putStrLn "\ESC[95m ------------------------------------------------------------------------------------------------------------"

  putStrLn "\ESC[93m ------------------------------------------------------------------------------------------------------------"
  putStrLn "\ESC[93m Movimiento de los ninnos . . ."
  c <- randomRIO (5, 1000 :: Int)
  let board_by_move = changeChilds board_by_environment n m r0 c0 rf cf c
  print__ board_by_move n m
  putStrLn "\ESC[93m ------------------------------------------------------------------------------------------------------------"

  putStrLn "\ESC[92m /////////////////////////////////////////////////////////////////////////////////////////////////////////////"
  putStrLn "\ESC[92m Cambiando el ambiente . . ."
  d <- randomRIO (8, 29098 :: Int)
  let board_by_time = changeEnvironment r0 c0 rf cf childs n m d
  print__ board_by_time n m
  putStrLn "\ESC[92m /////////////////////////////////////////////////////////////////////////////////////////////////////////////"

  putStrLn "\ESC[94m /////////////////////////////////////////////////////////////////////////////////////////////////////////////"


  agentStart (count - 1) t t board_by_time r0 c0 rf cf childs n m []


agentStart count t0 t board r0 c0 rf cf childs n m robots_up = do
  putStrLn "\ESC[94m Nuevo turno"
  putStrLn "\ESC[94m //////////////////////////////////////////////////////////////////////////////////////////////////////////"

  let home_status__ = homeClean board n m
  let board_by_environment:new_robots_up:_ = if home_status__ then [board, robots_up] else bdiStart board n m r0 c0 rf cf robots_up
  let home_status | home_status__ = "\ESC[97m La casa se encuentra limpia"
                  | otherwise = "\ESC[96m La casa se encuentra sucia"
  
  putStrLn home_status
  let agents_str  | home_status__ = "\ESC[95m Agentes esperando . . ."
                  | otherwise = "\ESC[95m Acciones de los agentes . . ."

  putStrLn "\ESC[95m ------------------------------------------------------------------------------------------------------------"
  putStrLn agents_str
  print__ board_by_environment n m
  putStrLn "\ESC[95m ------------------------------------------------------------------------------------------------------------"
  
  putStrLn "\ESC[93m ------------------------------------------------------------------------------------------------------------"
  putStrLn "\ESC[93m Movimiento de los ninnos . . ."
  c <- randomRIO (5, 1000 :: Int)
  let board_by_move = changeChilds board_by_environment n m r0 c0 rf cf c
  print__ board_by_move n m
  putStrLn "\ESC[93m ------------------------------------------------------------------------------------------------------------"

  putStrLn "\ESC[94m //////////////////////////////////////////////////////////////////////////////////////////////////////////"


  agentStart (count - 1) (t0 - 1) t board_by_move r0 c0 rf cf childs n m new_robots_up
--  agentStart (count - 1) (t0 - 1) t board_by_environment r0 c0 rf cf childs n m new_robots_up


homeClean::[[Int]] -> Int -> Int -> Bool
homeClean board n m = let
  dirts_pos = takePos board n m 2
  dirts_count = length dirts_pos
  in div (dirts_count * 100) (n * m) < 10


changeEnvironment:: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
changeEnvironment r0 c0 rf cf childs n m c = let
  board_shape = boardGenerateCorral n n m r0 c0 rf cf []
  board_childs = boardGenerateWithChilds n m r0 c0 rf cf board_shape childs c
  board_dirt = boardGenerateWithDirt n n m board_childs r0 c0 rf cf [] (c * 2)
  board_obstacles = boardGenerateWithObstacles n n m board_dirt r0 c0 rf cf (c + 13)
  in boardComplete n m r0 c0 rf cf board_obstacles (c * 6)

changeChilds :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
changeChilds board n m r0 c0 rf cf c = let
  childs = takePosChild board n m r0 c0 rf cf
  childs_in_corral = takePosChildInCorral board (r0 + 1) rf (c0 + 1) (c0 + 1) cf
  g = mkStdGen c
  (childs_count, f) = randomR (0, length childs :: Int) g
  childs_pos = posGen childs childs_count f
  childs_dir_new = takeMoveChildsDir board n m r0 c0 rf cf childs_pos (length childs_pos) (length childs_pos) f []
  childs_new = takeMoveChilds childs_dir_new (length childs_dir_new) (length childs_dir_new)
  obstacles_new = takePosObstacles board n n m m childs_dir_new
  childs_sort = sortTuple ((childs  ++ childs_new ++ childs_in_corral) \\ childs_pos)
  obstacles_sort = sortTuple obstacles_new
  new_board1 = boardChildsMove board n n m childs_sort (length childs_sort) (length childs_sort) obstacles_sort (length obstacles_sort) (length obstacles_sort)

  h = mkStdGen (c - 50)  
  dirt_pos = takeBoxes3x3 new_board1 1 n 1 m r0 c0 rf cf h
  dirt_sort = sortTuple dirt_pos
  new_board2 = boardWithDirt new_board1 n n m dirt_sort (length dirt_sort) (length dirt_sort)
  in new_board2
