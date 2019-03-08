import Data.String.Utils
import Data.Char

-- holy shit so slow :3

type Jumps = [Int]
type Cursor = Int

jump :: (Jumps, Cursor) -> (Jumps, Cursor)
jump (jumps, cursor) = (increaseJump cursor jumps, cursor + next)
  where
    next = (jumps !! cursor)

increaseJump :: Int -> Jumps -> Jumps
increaseJump index jumps = map f $ zip jumps [0..]
  where
    f (jump, i) = if i == index then (jump + 1) else jump

testInput :: [Int]
testInput = [0, 3, 0, 1, -3]

solve :: Jumps -> Int
solve jumps = length . takeWhile (\(js, i) -> i < length js) . iterate jump $ (jumps, 0)

main = do
  content <- readFile "input.txt"
  let input = map (read :: String->Int) . lines . strip $ content
  print $ solve input
