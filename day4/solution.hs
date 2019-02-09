import Data.String.Utils
import Data.List

isValid :: String -> Bool
isValid passphrase = (nub ws) == ws
  where ws = words passphrase

part1 :: [String] -> Int
part1 = length . filter isValid

main = do
  content <- readFile "input.txt"
  let input = lines . strip $ content
  putStrLn $ "Part1: " ++ (show $ part1 input)
