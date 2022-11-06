{-
   Copyright 2022 J. Walker Orr

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

module Style.Duplicate (findDuplicates, findSelectDuplicates) where 

import Data.List 
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)

--minimum length to check
minLength = 20

{- determines which parts of a text are duplicated, returns the lines that
have a duplicate -}
findDuplicates :: Float -> [String] -> [(Int, Int)]
findDuplicates threshold lines = 
   let
      --remove leading whitespace from each line
      cleanedLines = map (dropWhile isSpace) lines
   in
      dupHelper threshold 1 cleanedLines

{- Finds duplicates of selected lines, returns line indexes of orignal -}
findSelectDuplicates :: (String -> Bool) -> Float -> [String] -> [(Int,Int)]
findSelectDuplicates pred threshold lines =
   let
      --record each line number
      indexed               = zip [1..] lines
      
      --remove all lines that do not match the predicate
      (reducedIdx, select)  = unzip $ filter (pred . snd) indexed

      dups                  = findDuplicates threshold select

      --TODO replace with Map
      --look up the original index in the list of indexes
      origIdx i             = reducedIdx !! (i - 1)
   in
      [ (origIdx f, origIdx s) | (f,s) <- dups ]

--recursive helper
dupHelper :: Eq a => Float -> Int -> [[a]] -> [(Int, Int)]
dupHelper _ _ [] = []
dupHelper threshold index (line:rest) =
   let
      restDups = dupHelper threshold (index + 1) rest
      matches  = [(index, index + m + 1) | m <- checkMatch threshold line rest]
   in
      matches ++ restDups

{- determines if there is another indexed line that matches the given line -}
checkMatch :: Eq a => Float -> [a] -> [[a]] -> [Int]
checkMatch thres line rest = 
   let
      safeMax []   = 0
      safeMax nums = maximum nums
      len = length line
      toPercent = (/ fromIntegral len) . fromIntegral
   in
      if len > minLength then
         findIndices (> thres) $ map (toPercent . safeMax . subSeqLength line) rest
      else
         []


--NOTE: not being used 
{- finds all the matching subsequences between the twosequences -}
matchingSubsequences :: Eq a => [a] -> [a] -> [[a]]
matchingSubsequences first second =
   let
      matchCounts = subSeqLength first second
   in
      --get the reverse subsequences, undo the reverse, filter empty subsequences
      filter (not . null) . map reverse $ subs (reverse second) (reverse matchCounts)


{- determines the length of the subsequence matches -}
subSeqLength :: Eq a => [a] -> [a] -> [Int]
subSeqLength first second =
   let 
      zeroes = replicate (length second) 0 
   in
      buildMatrix zeroes zeroes first second
   

{- Constructs a matrix of matches e.g.

    b a r f o o
  b 1 0 0 0 0 0
  a 0 2 0 0 0 0
  z 0 0 0 0 0 0
  f 0 0 0 1 0 0
  o 0 0 0 0 2 1
  o 0 0 0 0 1 3

  Adding to the previous diagonal square if there is a match,
  then finds the longest diagonals in the matrix.
  In this example first sequence is bazfoo and the second is barfoo.
  Note that only the previous

  The result is a list of best subsequence length ending at that position.
  For the example above the output should be 1 2 0 1 2 3
-}
buildMatrix :: Eq a => [Int] -> [Int] -> [a] -> [a] -> [Int]
buildMatrix best _ [] _                        = best
buildMatrix best prevRow (current:rest) second =
   let
      --get the diagonal for each square in the new row (lag of 1)
      prevDiag = Nothing : (map Just prevRow) 

      --increment the previous
      seqCount = map (buildCell current) $ zip prevDiag second

      --update the best
      newBest = map (uncurry max) $ zip best seqCount

    in
      buildMatrix newBest seqCount rest second

{- builds the cell of the subsequence matrix -}
buildCell :: Eq a => a -> (Maybe Int, a) -> Int
buildCell current (diagCell, otherVal)
   | current == otherVal = (fromMaybe 0 diagCell) + 1
   | otherwise           = 0


{- Makes sub sequences based on the parallel count list -}
subs :: [a] -> [Int] -> [[a]]
subs [] [] = []
subs values (n:rest) = (take n values) : subs (tail values) rest
      
