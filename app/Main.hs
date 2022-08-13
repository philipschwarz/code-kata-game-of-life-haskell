module Main (main) where

import Lib

---------------------------------------------------------------------------
-- Functional Core
---------------------------------------------------------------------------

width = 20
height = 20

type Pos = (Int,Int)

type Board = [Pos]

pulsar :: Board
pulsar = [(4, 2),(5, 2),(6, 2),(10, 2),(11, 2),(12, 2),
                 (2, 4),(7, 4),( 9, 4),(14, 4),
                 (2, 5),(7, 5),( 9, 5),(14, 5),
                 (2, 6),(7, 6),( 9, 6),(14, 6),
          (4, 7),(5, 7),(6, 7),(10, 7),(11, 7),(12, 7),
          (4, 9),(5, 9),(6, 9),(10, 9),(11, 9),(12, 9),
                 (2,10),(7,10),( 9,10),(14,10),
                 (2,11),(7,11),( 9,11),(14,11),
                 (2,12),(7,12),( 9,12),(14,12),
          (4,14),(5,14),(6,14),(10,14),(11,14),(12,14)]

isAlive :: Board -> Pos -> Boolean
isAlive b p = elem p b

isDead :: Board -> Pos -> Boolean
isDead b p = not (isAlive b p)

neighbours :: Pos -> [Pos]
neighbours (x,y) = map wrap [(x-1, y-1),(x, y-1),(x+1, y-1),
                             (x-1, y),          ,(x+1, y),
                             (x-1, y+1),(x, y+1),(x+1, y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

liveNeighbours :: Board -> Pos -> Int
liveNeighbours b = 
  length . filter (isAlive b) . neighbours

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveNeighbours b p) [2,3] ]
   
newborns :: Board -> [Pos] 
newborns b = [(x,y) | x <- [1..width],
                      y <- [1..height],
                      isDead b (x,y),
                      liveNeighbours b (x,y) == [3]]

nextGen :: Board -> Board
  survivors b ++ newborns b

---------------------------------------------------------------------------
-- Imperative Shell
---------------------------------------------------------------------------

goto :: Pos -> IO ()
goto (x,y) = putString "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

writeAt :: Pos -> String -> IO ()
writeAt pos xs = do
  goto pos
  putStr xs

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

showcells :: Board -> IO ()
showcells b = sequence_ [writeAt p "O" | p <- b]

cls :: IO ()
cls = putsStr "\ESC[2J"

life :: Board -> IO ()
life b = do
  cls
  showcells b
  wait 500000
  life (nextGen b)
  
main :: IO ()
main = life(pulsar)
