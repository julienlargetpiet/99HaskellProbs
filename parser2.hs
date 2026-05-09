import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

getRangeList :: [a] -> [Int] -> [a]
getRangeList [] _ = []
getRangeList _ [] = []
getRangeList xs (idx:ids) = (xs !! idx):(getRangeList xs ids) 

grepn2 :: (Eq a) => a -> [a] -> [Int]
grepn2 cmp xs = subGrepn2 xs cmp 0 []

subGrepn2 :: (Eq a) => [a] -> a -> Int -> [Int] -> [Int]
subGrepn2 [] _ _ nxs = nxs
subGrepn2 (x:xs) cmp n nxs
    | cmp == x  = subGrepn2 xs cmp (n + 1) (n:nxs)
    | otherwise = subGrepn2 xs cmp (n + 1) nxs

grepmn2 :: (Eq a) => [a] -> [a] -> [Int]
grepmn2 [] _ = []
grepmn2 (x2:xs2) xs = (grepn2 x2 xs) ++ (grepmn2 xs2 xs)

myMax :: (Ord a) => [a] -> a
myMax xs = subMyMax xs (head xs)

subMyMax :: (Ord a) => [a] -> a -> a
subMyMax [] cmp = cmp
subMyMax (x:xs) cmp = 
    let cmp2 = if cmp >= x
              then cmp
              else x
    in subMyMax xs cmp2

parserPar :: ByteString -> ([Int], [Int])
parserPar xs = subParserPar xs [] [] [] 0 0

subParserPar :: ByteString -> [Int] -> [Int] -> [Int] -> Int -> Int
             -> ([Int], [Int])
subParserPar bs ids nums valxs n n2 =
    case B.uncons bs of
        Nothing ->
            (ids, nums)

        Just (x, xs)
            | x == '(' ->
                let newids = ids ++ [n]
                    newnums = nums ++ [n2]
                    newvalxs = map (\v -> v + 1) valxs
                    newvalxs2 = newvalxs ++ [1]
                in subParserPar xs newids newnums newvalxs2 (n + 1) (n2 + 1)

            | x == ')' ->
                let newvalxs = map (\v -> v - 1) valxs
                    idx = findFirstZero (reverse newvalxs) 0
                    idx2 = length valxs - idx - 1
                    newids = ids ++ [n]
                    newnums = nums ++ [nums !! idx2]
                in subParserPar xs newids newnums (newvalxs ++ [0]) (n + 1) n2

            | otherwise ->
                subParserPar xs ids nums valxs (n + 1) n2

findFirstZero :: [Int] -> Int -> Int
findFirstZero (xi:xsi) n
              | xi == 0 = n
              | otherwise = findFirstZero xsi (n + 1)



