import qualified Data.Set as Set
import Control.DeepSeq (deepseq)

mySequence :: [[a]] -> [[a]]
mySequence xs = 
    let lxs = mySequencePrepareLength xs
        ids = mySequencePrepareIds xs
    in [mySequenceList xs lids | lids <- mySequenceIdsn lxs ids l]
    where l = length xs - 1

mySequenceList :: [[a]] -> [Int] -> [a]
mySequenceList _ [] = []
mySequenceList [] _ = []
mySequenceList (x:xs) (idx:ids) = (x !! idx):(mySequenceList xs ids)

mySequencePrepareLength :: [[a]] -> [Int]
mySequencePrepareLength [] = []
mySequencePrepareLength (x:xs) = (length x):(mySequencePrepareLength xs)

mySequencePrepareIds :: [[a]] -> [Int]
mySequencePrepareIds [_] = (-1):[]
mySequencePrepareIds (_:xs) = 0:(mySequencePrepareIds xs)

mySequenceIdsn :: [Int] -> [Int] -> Int -> [[Int]]
mySequenceIdsn lxs ids idx
    | idx == 0 = if val == (cmp - 1)
                 then []
                 else let newids = subMySequence 0 ids 0
                          newids2 = subMySequence2 (length lxs - 1) newids 0
                      in mySequenceIdsn lxs newids2 (length lxs - 1)
    | val < cmp - 1 = 
        let newids = subMySequence idx ids 0
        in  newids:(mySequenceIdsn lxs newids (length lxs - 1))
    | otherwise = 
        let newids = subMySequence3 idx ids 0
        in  mySequenceIdsn lxs newids (idx - 1)
    where val = (ids !! idx)
          cmp = (lxs !! idx)

subMySequence :: Int -> [Int] -> Int -> [Int]
subMySequence _ [] _ = []
subMySequence idx (x:xs) n = if idx /= n
                             then x:(subMySequence idx xs (n + 1))
                             else (x + 1):(subMySequence idx xs (n + 1))

subMySequence2 :: Int -> [Int] -> Int -> [Int]
subMySequence2 _ [] _ = []
subMySequence2 idx (x:xs) n = if idx /= n
                              then x:(subMySequence2 idx xs (n + 1))
                              else (-1):(subMySequence2 idx xs (n + 1))

subMySequence3 :: Int -> [Int] -> Int -> [Int]
subMySequence3 _ [] _ = []
subMySequence3 idx (x:xs) n = if idx /= n
                              then x:(subMySequence3 idx xs (n + 1))
                              else 0:(subMySequence3 idx xs (n + 1))

uniqueOrd :: Ord a => [a] -> [a]
uniqueOrd xss = go Set.empty xss
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs


mySequence2 :: [[a]] -> [[a]]
mySequence2 [] = [[]]
mySequence2 (xs:xss) = [x:ys | x <- xs, ys <- mySequence2 xss]

benchmarkSequence1 :: [[a]] -> Int -> Int
benchmarkSequence1 xs 1 = length $ mySequence xs
benchmarkSequence1 xs n = 
    let r = length $ mySequence xs
    in r `deepseq` benchmarkSequence1 xs (n - 1)

benchmarkSequence2 :: [[Int]] -> Int -> Int
benchmarkSequence2 xs 1 =
    let r = mySequence2 xs
    in r `deepseq` length r
benchmarkSequence2 xs n =
    let r = mySequence2 xs
    in r `deepseq` benchmarkSequence2 xs (n - 1)

--benchmarkSequenceNative :: [[a]] -> Int -> Int
--benchmarkSequenceNative xs 1 = length $ sequence xs
--benchmarkSequenceNative xs n = 
--    let r = length $ sequence xs
--    in r `seq` benchmarkSequenceNative xs (n - 1)

benchmarkSequenceNative :: [[Int]] -> Int -> Int
benchmarkSequenceNative xs 1 =
    let r = sequence xs
    in r `deepseq` length r
benchmarkSequenceNative xs n =
    let r = sequence xs
    in r `deepseq` benchmarkSequenceNative xs (n - 1)

main :: IO ()
main =
    let inpt =
            [ [0,  1,  2,  3,  4]  
            , [10, 11, 12, 13, 14] 
            , [20, 21, 22, 23, 24] 
            , [30, 31, 32, 33, 34] 
            , [40, 41, 42, 43, 44] 
            ]
        iter = 100000
    in print $ benchmarkSequenceNative inpt iter

--main :: IO ()
--main = 
--    let inpt = [[1, 2], [3, 4, 5], [6, 7]]
--        iter = 1000000
--    in print $ benchmarkSequenceNative inpt iter



