import Data.List
import qualified Data.Set as S

-- it finds the memory bank with the most blocks (ties won by the lowest-numbered memory bank)
-- redistributes those blocks among the banks
--    removes all of the blocks from the selected bank
--    then moves to the next (by index) memory bank and inserts one of the blocks
--
--
test :: [Int]
test =  [0, 2, 7, 0]

-- Find the index of the bank to distribute
bankToDistribute :: [Int] -> Int
bankToDistribute banks =
  let indices = zip banks [0..]
   in snd $ maximumBy (\(a,_)(b,_) -> compare a b) . reverse $ indices

-- Clear the value of the bank at an index
clearBankAt :: [Int] -> Int -> [Int]
clearBankAt banks i =
  let withIndices = zip banks [0..]
   in map (\(value, index) -> if index == i then 0 else value) withIndices

-- add one to a bank at an index
addMemoryAt :: [Int] -> Int -> [Int]
addMemoryAt banks i =
  let withIndices = zip banks [0..]
   in map (\(value, index) -> if index == i then value + 1 else value) withIndices

-- distribute an amount to the banks, starting at the given index
distribute :: [Int] -> Int -> Int -> [Int]
distribute banks value start =
  let initialState = (banks, value, start)
   in bankFromState . last . take (value + 1) $ iterate distribution initialState
  where
    distribution (bs, val, pos) = (addMemoryAt bs pos, val - 1, (pos + 1) `mod` length banks)
    bankFromState (b, _, _) = b

-- iterateWhile :: (a -> a) -> (a -> Bool) -> a -> [a]
-- iterateWhile func cond = takeWhile cond . iterate func

-- TODO: Something is wrong with the distribution logic. pls fix lel
-- solve :: [Int] -> [[Int]]
solve banks =
  let initialState = (banks, S.fromList [banks], False)
   in takeWhile (\(_,_,stop) -> stop == False) $ iterate reducer initialState
  where
    reducer (banks, seen, stopCondition) =
      let distributor = bankToDistribute banks
          newBanks = distribute (clearBankAt banks distributor) (maximum banks) (distributor + 1)
          shouldStop = S.member newBanks seen
       in (newBanks, S.insert newBanks seen, shouldStop)
-- redistribution
-- choose bank to distribute
-- set this bank to 0
-- distribute the points
