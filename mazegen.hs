-- Maze generator

import System.Random
import Data.List

-- Helper: Construct an array of length n with [f 0, f 1, ... f n-1]
igen f n = map f [0..n-1]

-- Helper: Return list without the element given
except a = filter (\x -> x /= a)

-- Helper: remove the segment [from..to] out of a list
removeSegment from to xs = (take from xs) ++ (drop (to+1) xs)

-- Helper: similar to a fold but function decides how many pieces of the array it consumes
collect f v [] = v
collect f v a = collect f v' a'
    where (v', a') = f v a

-- Helper: shuffle an array
shuffle rng src =
    (calcResult src, rng'')
    where
    (rng', rng'') = split rng
    calcResult = snd . unzip . sort . zip (randoms rng' ::[Int])

-- Maze and Cell types
data Cell = Start | Exit | Empty | Solid deriving (Eq)

data Maze = Maze { w :: Int, h :: Int, cells :: [Cell] }

-- Generate a maze with all solid except start and exit points
initMaze w h (sx, sy) (ex, ey) = 
    Maze {w=w, h=h, cells=cells}
    where
        cells = igen setCell (w*h)
        setCell i | i == (sx+sy*w) = Start
                  | i == (ex+ey*w) = Exit
                  | otherwise  = Solid

-- Various maze cell accessors
mazeCell maze x y =
    (cells maze) !! (x+y*(w maze))

isCellValid maze x y =
    x >= 0 && y >= 0 && x < (w maze) && y < (h maze)

isCellSolid maze x y =
    (isCellValid maze x y) && (mazeCell maze x y) /= Empty

-- Cell can be carved for our maze if it's solid and all neighbours are too
-- Excluding the neighbours we're coming from, which has been carved already
isCellCarveable maze x y px py =
    all isNeighbourSolid $ except (px,py) [(x,y),(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
    where
        isNeighbourSolid (x,y) = isCellSolid maze x y

-- Carve rooms in the maze until no more
-- The stack contains the new point to try, and the point it's coming from
carveMaze rng maze [] = maze
carveMaze rng maze@(Maze w h cells) (((x,y),(px,py)):stack)
    | not $ isCellCarveable maze x y px py = carveMaze rng maze stack
    | otherwise = carveMaze rng' maze' stack'
        where
            -- Push all three neighbours into the stack in random order
            (neighbours, rng') = shuffle rng (except (px,py) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)])
            stack' = [(a,(x,y)) | a <- neighbours ] ++ stack
            -- Chop the current cell
            maze' = Maze w h $ igen setCell (w*h)
            setCell i | i == (x+y*w) && (cells !! i) == Solid = Empty
                      | otherwise = cells !! i

-- Build an solid area then carve a maze starting from the start point
genMaze rng w h (sx, sy) (ex, ey) =
    carveMaze rng maze [((sx+1, sy),(sx, sy))]
    where
        maze = initMaze w h (sx, sy) (ex, ey)

-- Representation of each cell type
strCell Start = '+'
strCell Exit  = '-'
strCell Empty = ' '
strCell Solid = '#'

strMaze maze = unlines $ collect func [] $ map strCell (cells maze)
    where func str cells = (str ++ [line], cells')
              where (line, cells') = splitAt (w maze) cells

main = do
    rng <- newStdGen
    let maze = genMaze rng 40 40 (0, 4) (39, 4)
    let ms = strMaze maze
    putStrLn ms 
    putStrLn "yey"
