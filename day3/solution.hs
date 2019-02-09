data Direction = N | E | S | W deriving (Show)

directionToVec :: Direction -> (Int, Int)
directionToVec N = (0, 1)
directionToVec E = (1, 0)
directionToVec S = (0, -1)
directionToVec W = (-1, 0)

left :: Direction -> Direction
left N = W
left E = N
left S = E
left W = S

vadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vadd (x,y) (i, j) = (x + i, y + j)

vdist :: (Int, Int) -> (Int, Int) -> Int
vdist (x, y) (i, j) = abs (x - i) + abs (y - j)

-- return the positions walked on from walking `n` step in `dir` direction
walk :: (Int, Int) -> Int -> Direction -> [(Int, Int)]
walk start n dir = init . scanr f start . take n $ repeat dir
  where
    f dir pos = vadd pos (directionToVec dir)

spiral :: Int -> [(Int, Int)]
spiral n = take n . reverse . snd . foldl fold (E, [(0, 0)]) $ moves [1..n]
  where
    fold = (\(dir, (p:ps)) n -> (left dir, (walk p n dir) ++ (p:ps)))
    moves = concat . map (\x -> [x, x])

part1 n = vdist (0, 0) . last $ spiral n


main = do
  content <- readFile "input.txt"
  let input = read content :: Int
  putStrLn $ show $ part1 input
