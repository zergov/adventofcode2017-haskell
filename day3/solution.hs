import qualified Data.Set as S
import qualified Data.Map as M

data Direction = N | E | S | W deriving (Show)

move :: Direction -> (Int, Int) -> (Int, Int)
move N (x, y) = (x, y + 1)
move E (x, y) = (x + 1, y)
move S (x, y) = (x, y - 1)
move W (x, y) = (x - 1, y)

left :: Direction -> Direction
left N = W
left E = N
left S = E
left W = S

vdist :: (Int, Int) -> (Int, Int) -> Int
vdist (x, y) (i, j) = abs (x - i) + abs (y - j)

-- turn left, whenever the left position from current position has not been seen
spiral :: [(Int, Int)]
spiral = map (\(p, _, _) -> p) $ iterate next ((0, 0), S, S.singleton (0, 0))
  where
    next (pos, dir, seen) =
      if (move (left dir) pos) `S.notMember` seen
        then (move (left dir) pos, left dir, S.insert (move (left dir) pos) seen)
        else (move dir pos, dir, S.insert (move dir pos) seen)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  [
    (x + 1, y)
  , (x + 1, y + 1)
  , (x, y + 1)
  , (x - 1, y + 1)
  , (x - 1, y)
  , (x - 1, y - 1)
  , (x, y - 1)
  , (x + 1, y - 1)
  ]

sumspiral :: [Int]
sumspiral = f M.empty spiral
  where
    f sums ((0,0):ps) = 1 : f (M.insert (0, 0) 1 sums) ps
    f sums (p:ps) = val : f (M.insert p val sums) ps
      where
        val = sum $ sum <$> (`M.lookup` sums) <$> (neighbors p)

part1 :: Int -> Int
part1 n = vdist (0, 0) $ spiral !! (n-1)

part2 :: Int -> Int
part2 n = head . dropWhile (< n) $ sumspiral

main = do
  content <- readFile "input.txt"
  let input = read content :: Int
  putStrLn $ "Part1: " ++ (show $ part1 input)
  putStrLn $ "Part2: " ++ (show $ part2 input)
