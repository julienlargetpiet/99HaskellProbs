import qualified System.Random as R
import Data.List (find)
import qualified Data.Array as A
import Data.List (sortOn)


-- helpers

-- Bonus question, right a calculator

mySplitAt :: (Eq a) => [a] -> [a] -> ([a], [a])
mySplitAt xs cmp = subMySplitAt xs [] cmp

subMySplitAt :: (Eq a) => [a] -> [a] -> [a] -> ([a], [a])
subMySplitAt (x:xs) outxs cmp
    | x `elem` cmp = (reverse outxs, xs)
    | otherwise = subMySplitAt xs (x:outxs) cmp

tailn :: Int -> [a] -> [a]
tailn n xs 
    | n == 0 = xs
    | otherwise = subTailn n xs 1

subTailn :: Int -> [a] -> Int -> [a]
subTailn cmp (_:xs) n
    | n < cmp = subTailn cmp xs (n + 1)
    | otherwise = xs

initn :: Int -> [a] -> [a]
initn n xs 
    | n == 0 = xs
    | otherwise = reverse $ subInitn n (reverse xs) 1

subInitn :: Int -> [a] -> Int -> [a]
subInitn cmp (_:xs) n
    | n < cmp = subInitn cmp xs (n + 1)
    | otherwise = xs

calc :: [Char] -> [Char]
calc xs = 
    let (ids, nums) = parserPar xs
        newxs = subCalc xs ids nums
    in protoCalc newxs
    
subCalc :: [Char] -> [Int] -> [Int] -> [Char]
subCalc xs [] [] = xs
subCalc xs ids nums = 
    let curmax = myMax nums
        [id1, id2] = grepn2 curmax nums
        idstrt = (ids !! id2)
        idstop = (ids !! id1)
        xsstrt = if idstrt > 0
                 then getRangeList xs [0..(idstrt - 1)]
                 else []
        xsstop = if idstop + 1 < length xs
                 then getRangeList xs [(idstop + 1)..(length xs - 1)]
                 else []
        xsbetween = getRangeList xs [(idstrt + 1)..(idstop - 1)]
        rslt = protoCalc xsbetween
        newxs = if head rslt /= '-'
                then xsstrt ++ rslt ++ xsstop
                else (getRangeList xsstrt [0..(length xsstrt) - 2]) ++ rslt ++ xsstop
        (newids, newnums) = parserPar newxs
    in subCalc newxs newids newnums

protoCalc :: [Char] -> [Char]
protoCalc xs = 
    let outxs = subProtoCalc2 (subProtoCalc xs []) [] 0
    in outxs

takeBack :: [Char] -> [Char]
takeBack [] = []
takeBack (x:xs) 
    | not (x `elem` "+-*/") = (x:takeBack xs)
    | otherwise = []

takeTailN :: [Char] -> [Char]
takeTailN [] = []
takeTailN (x:xs)
    | not (x `elem` "+-*/") = takeTailN xs
    | otherwise = x:xs

subProtoCalc :: [Char] -> [Char] -> [Char]
subProtoCalc [] outxs = outxs
subProtoCalc (x:xs) outxs
    | x == '*' = 
        if head xs /= '-'
        then let val1 = read . reverse . takeBack . reverse $ outxs
                 val2 = read . takeBack $ xs
                 newoutxs = reverse . takeTailN . reverse $ outxs
                 newxs = takeTailN xs
             in subProtoCalc newxs (newoutxs ++ (show (val1 * val2)))
        else let xsb = tail xs
                 val1 = read . reverse . takeBack . reverse $ outxs
                 val2 = read . takeBack $ xsb
                 newoutxs = reverse . takeTailN . reverse $ outxs
                 newxs = takeTailN xsb
             in  if null newoutxs
                 then subProtoCalc newxs (newoutxs ++ (show (-val1 * val2)))
                 else if last newoutxs /= '-'
                     then subProtoCalc newxs (init newoutxs ++ (show (-val1 * val2)))
                     else subProtoCalc newxs (init newoutxs ++ "+" ++ (show (val1 * val2)))
    | x == '/' = 
        if head xs /= '-'
        then let val1 = read . reverse . takeBack . reverse $ outxs
                 val2 = read . takeBack $ xs
                 newoutxs = reverse . takeTailN . reverse $ outxs
                 newxs = takeTailN xs
             in subProtoCalc newxs (newoutxs ++ (show (val1 `div` val2)))
        else let xsb = tail xs
                 val1 = read . reverse . takeBack . reverse $ outxs
                 val2 = read . takeBack $ xsb
                 newoutxs = reverse . takeTailN . reverse $ outxs
                 newxs = takeTailN xsb
             in if null newoutxs
                then subProtoCalc newxs (newoutxs ++ (show (-val1 `div` val2)))
                else if last newoutxs /= '-'
                    then subProtoCalc newxs (init newoutxs ++ (show (-val1 `div` val2)))
                    else subProtoCalc newxs (init newoutxs ++ "+" ++ (show (val1 `div` val2)))
    | otherwise = subProtoCalc xs (outxs ++ [x])

clearMinus :: [Char] -> [Char]
clearMinus xs = subClearMinus xs 0

subClearMinus :: [Char] -> Int -> [Char]
subClearMinus (x:xs) n
    | x /= '-' = if n `mod` 2 /= 0
                 then  '-':x:xs
                 else x:xs
    | otherwise = subClearMinus xs (n + 1)

subProtoCalc2 :: [Char] -> [Char] -> Int -> [Char]
subProtoCalc2 [] outxs _ = outxs
subProtoCalc2 (x:xs) outxs n
    | x == '+' = 
        let val1 = read . reverse . takeBack . reverse $ outxs
            val2 = read . takeBack $ xs
            newoutxs = reverse . takeTailN . reverse $ outxs
            newxs = takeTailN xs
        in if null newoutxs 
           then subProtoCalc2 newxs (newoutxs ++ (show (val1 + val2))) (n + 1)
           else if last newoutxs /= '-'
           then subProtoCalc2 newxs (newoutxs ++ (show (val1 + val2))) (n + 1)
           else if val1 > val2
                then subProtoCalc2 newxs (newoutxs ++ (show (val1 - val2))) (n + 1)
                else subProtoCalc2 newxs (init newoutxs ++ (show (val2 - val1))) (n + 1)
    | x == '-' && n /= 0 = 
        let val1 = read . reverse . takeBack . reverse $ outxs
            val2 = read . takeBack $ xs
            newoutxs = reverse . takeTailN . reverse $ outxs
            newxs = takeTailN xs
        in if null newoutxs 
           then subProtoCalc2 newxs (newoutxs ++ (show (val1 - val2))) (n + 1)
           else if last newoutxs /= '-'
           then subProtoCalc2 newxs (newoutxs ++ (show (val1 - val2))) (n + 1)
           else subProtoCalc2 newxs (newoutxs ++ (show (val1 + val2))) (n + 1)
    | otherwise = subProtoCalc2 xs (outxs ++ [x]) (n + 1)


parserPar :: [Char] -> ([Int], [Int])
parserPar xs = subParserPar xs [] [] [] 0 0

subParserPar :: [Char] -> [Int] -> [Int] -> [Int] -> Int -> Int
                -> ([Int], [Int])
subParserPar [] ids nums _ _ _ = (ids, nums)
subParserPar (x:xs) ids nums valxs n n2
    | x == '(' = 
        let newids = ids ++ [n]
            newnums = nums ++ [n2]
            newvalxs = map (\x -> x + 1) valxs
            newvalxs2 = newvalxs ++ [1]
        in subParserPar xs newids newnums newvalxs2 (n + 1) (n2 + 1)
    | x == ')' = 
        let newvalxs = map (\x -> x - 1) valxs 
            idx = findFirstZero (reverse newvalxs) 0
            idx2 = (length valxs) - idx - 1
            newids = ids ++ [n]
            newnums = nums ++ [(nums !! idx2)]
        in subParserPar xs newids newnums (newvalxs ++ [0]) (n + 1) n2
    | otherwise = subParserPar xs ids nums valxs (n + 1) n2

findFirstZero :: [Int] -> Int -> Int
findFirstZero (xi:xsi) n
              | xi == 0 = n
              | otherwise = findFirstZero xsi (n + 1)


groupIdx :: [Int] -> [a] -> [[a]]
groupIdx [] _ = []
groupIdx _ [] = []
groupIdx (idx:ids) xs = 
    let (outxs, newxs) = subGroupIdx idx 0 [] xs
    in outxs:(groupIdx ids newxs)

subGroupIdx :: Int -> Int -> [a] -> [a] -> ([a], [a])
subGroupIdx cmp n outxs [] = (reverse outxs, [])
subGroupIdx cmp n outxs (x:xs)
    | n < cmp   = subGroupIdx cmp (n + 1) (x:outxs) xs
    | otherwise = (reverse outxs, (x:xs))

sortAccordinglyAsc :: (Ord a, Eq a) => [a] -> [b] -> [b]
sortAccordinglyAsc valxs xs = 
    let newvalxs = quickSortAsc valxs
        ids = grepIds newvalxs valxs
        newxs = getRangeList xs ids
    in newxs
    where grepIds [] _ = []
          grepIds (x:xs) xs2 = (grep2 x xs2):grepIds xs xs2

sortAccordinglyDesc :: (Ord a, Eq a) => [a] -> [b] -> [b]
sortAccordinglyDesc valxs xs = 
    let newvalxs = quickSortAsc valxs
        ids = grepIds valxs newvalxs
        newxs = getRangeList xs ids
    in newxs
    where grepIds [] _ = []
          grepIds (x:xs) xs2 = (grep2 x xs2):grepIds xs xs2

permu :: [a] -> [[a]]
permu xs = subPermu [xs] 0 (length xs - 1)

subPermu :: [[a]] -> Int -> Int -> [[a]]
subPermu xs n cmp
    | n < cmp = subPermu (concat $ map (\x -> permutationAt x n) xs) (n + 1) cmp
    | otherwise = xs

permutationAt :: [a] -> Int -> [[a]]
permutationAt xs n = subPermutationAt xs n n

subPermutationAt :: [a] -> Int -> Int -> [[a]]
subPermutationAt xs n nb
    | n < l = (subPermu3 xs n nb (xs !! nb) (xs !! n) 0):subPermutationAt xs (n + 1) nb
    | otherwise = []
    where l = length xs

subPermu3 :: [a] -> Int -> Int -> a -> a -> Int -> [a]
subPermu3 [] _ _ _ _ _ = []
subPermu3 (x:xs) n nb val val2 n2
    | n2 == nb   = val2:subPermu3 xs n nb val val2 (n2 + 1)
    | n2 /= n   = x:subPermu3 xs n nb val val2 (n2 + 1)
    | otherwise = val:subPermu3 xs n nb val val2 (n2 + 1)


breakAt :: (Eq a) => [(a, a)] -> [[(a, a)]]
breakAt xs = 
    let uniquevals = unique $ map (\(val, _) -> val) xs
    in subBreakAt uniquevals xs

subBreakAt :: (Eq a) => [a] -> [(a, a)] -> [[(a, a)]]
subBreakAt [] _ = []
subBreakAt (x:xs) xs2 = [filter (\(val, _) -> val==x) xs2] ++ subBreakAt xs xs2

myAny :: [Bool] -> Bool
myAny [] = False
myAny (x:xs)
    | x = True
    | otherwise = myAny xs

myAll :: [Bool] -> Bool
myAll [] = True
myAll (x:xs)
    | not x = False
    | otherwise = myAll xs

stopAt :: (Eq a) => a -> Int -> [a] -> Int
stopAt cmp n xs = subStopAt cmp n xs 0 0

subStopAt :: (Eq a) => a -> Int -> [a] -> Int -> Int -> Int
subStopAt _ _ [] _ _ = -1
subStopAt cmp n (x:xs) n2 n3
    | cmp == x = if n2 + 1 == n
                 then n3
                 else subStopAt cmp n xs (n2 + 1) (n3 + 1)
    | otherwise = subStopAt cmp n xs n2 (n3 + 1)

unique :: (Eq a) => [a] -> [a]
unique xs = subUnique xs []

subUnique :: (Eq a) => [a] -> [a] -> [a]
subUnique [] xs2 = xs2
subUnique (x:xs) xs2
    | x `elem` xs2 = subUnique xs xs2
    | otherwise = subUnique xs (x:xs2)

myMin :: (Ord a) => [a] -> a
myMin xs = subMyMin xs (head xs)

subMyMin :: (Ord a) => [a] -> a -> a
subMyMin [] cmp = cmp
subMyMin (x:xs) cmp = 
    let cmp2 = if cmp <= x
              then cmp
              else x
    in subMyMin xs cmp2

myMax :: (Ord a) => [a] -> a
myMax xs = subMyMax xs (head xs)

subMyMax :: (Ord a) => [a] -> a -> a
subMyMax [] cmp = cmp
subMyMax (x:xs) cmp = 
    let cmp2 = if cmp >= x
              then cmp
              else x
    in subMyMax xs cmp2

myMinN :: (Ord a) => [a] -> Int -> [a]
myMinN xs cmpn = subMyMinN xs 0 cmpn []

subMyMinN :: (Ord a) => [a] -> Int -> Int -> [a] -> [a]
subMyMinN [] _ _ xs2 = xs2
subMyMinN xs n cmpn xs2
    | n < cmpn = 
        let val = myMin xs
            ids = grepn2 val xs
            newxs = deleteListElemn xs (reverse ids)
            newxs2 = val:xs2
        in subMyMinN newxs (n + 1) cmpn newxs2
    | otherwise = xs2

myMinN2 :: (Ord a) => [a] -> Int -> [a]
myMinN2 xs cmpn = subMyMinN2 xs 0 cmpn []

subMyMinN2 :: (Ord a) => [a] -> Int -> Int -> [a] -> [a]
subMyMinN2 [] _ _ xs2 = xs2
subMyMinN2 xs n cmpn xs2
    | n < cmpn = 
        let val = myMin xs
            idx = match2 val xs
            newxs = deleteListElem xs idx
            newxs2 = val:xs2
        in subMyMinN2 newxs (n + 1) cmpn newxs2
    | otherwise = xs2

match :: (Eq a) => a -> [a] -> Maybe Int
match cmp xs = subMatch xs cmp 0

subMatch :: (Eq a) => [a] -> a -> Int -> Maybe Int
subMatch [] _ _ = Nothing
subMatch (x:xs) cmp n
    | x == cmp = Just n
    | otherwise = subMatch xs cmp (n + 1)

match2 :: (Eq a) => a -> [a] -> Int
match2 cmp xs = subMatch2 xs cmp 0

subMatch2 :: (Eq a) => [a] -> a -> Int -> Int
subMatch2 [] _ _ = -1
subMatch2 (x:xs) cmp n
    | x == cmp = n
    | otherwise = subMatch2 xs cmp (n + 1)

deleteListElem :: [a] -> Int -> [a]
deleteListElem xs n = subDeleteListElem xs n 0

subDeleteListElem :: [a] -> Int -> Int -> [a]
subDeleteListElem [] _ _ = []
subDeleteListElem (x:xs) cmp n
    | n /= cmp = x:(subDeleteListElem xs cmp (n + 1))
    | otherwise = subDeleteListElem xs cmp (n + 1)

deleteListElemn :: [a] -> [Int] -> [a]
deleteListElemn xs nxs = subDeleteListElemn xs nxs 0

subDeleteListElemn :: [a] -> [Int] -> Int -> [a]
subDeleteListElemn xs [] _ = xs
subDeleteListElemn xs (cmp:cmpxs) n = 
    let newxs = subDeleteListElem xs (cmp - n) 0
    in subDeleteListElemn newxs cmpxs (n + 1)

updateListElem :: [a] -> Int -> a -> [a]
updateListElem xs ncmp val = subUpdateListElem xs 0 ncmp val

subUpdateListElem :: [a] -> Int -> Int -> a -> [a]
subUpdateListElem [] _ _ _ = []
subUpdateListElem (x:xs) n ncmp val
    | n /= ncmp = x:(subUpdateListElem xs (n + 1) ncmp val)
    | otherwise = val:(subUpdateListElem xs (n + 1) ncmp val)

myMinIdx :: (Ord a) => [a] -> (a, Int)
myMinIdx xs = subMyMinIdx xs (head xs) 0 0

subMyMinIdx :: (Ord a) => [a] -> a -> Int -> Int -> (a, Int)
subMyMinIdx [] cmp n _ = (cmp, n)
subMyMinIdx (x:xs) cmp n n2 = 
    let (newcmp, newn) = if cmp <= x
                         then (cmp, n)
                         else (x, n2)
    in subMyMinIdx xs newcmp newn (n2 + 1)

myMinIdxN :: [Int] -> Int -> [(Int, Int)]
myMinIdxN xs n = subMyMinIdxNB xs n 0 []

subMyMinIdxNB :: [Int] -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
subMyMinIdxNB xs cmpn n xs2
    | n < cmpn = 
        let (minval, idxval) = subMyMinIdx xs (head xs) 0 0
            newxs = updateListElem xs idxval (maxBound :: Int)
        in subMyMinIdxNB newxs cmpn (n + 1) ((minval, idxval):xs2)
    | otherwise = xs2

myMinIdx2 :: (Ord a) => [a] -> Int
myMinIdx2 xs = subMyMinIdx2 xs (head xs) 0 0

subMyMinIdx2 :: (Ord a) => [a] -> a -> Int -> Int -> Int
subMyMinIdx2 [] _ n _ = n
subMyMinIdx2 (x:xs) cmp n n2 = 
    let (newcmp, newn) = if cmp <= x
                         then (cmp, n)
                         else (x, n2)
    in subMyMinIdx2 xs newcmp newn (n2 + 1)

grep :: (Eq a) => a -> [a] -> Maybe Int
grep cmp xs = subGrep xs cmp 0

subGrep :: (Eq a) => [a] -> a -> Int -> Maybe Int
subGrep [] _ _ = Nothing
subGrep (x:xs) cmp n
    | cmp == x = Just n
    | otherwise = subGrep xs cmp (n + 1)

grep2 :: (Eq a) => a -> [a] -> Int
grep2 cmp xs = subGrep2 xs cmp 0

subGrep2 :: (Eq a) => [a] -> a -> Int -> Int
subGrep2 [] _ _ = -1
subGrep2 (x:xs) cmp n
    | cmp == x = n
    | otherwise = subGrep2 xs cmp (n + 1)

grepn :: (Eq a) => a -> [a] -> Maybe [Int]
grepn cmp xs = subGrepn xs cmp 0 []

subGrepn :: (Eq a) => [a] -> a -> Int -> [Int] -> Maybe [Int]
subGrepn [] _ _ nxs = if null nxs then Nothing else Just nxs
subGrepn (x:xs) cmp n nxs
    | cmp == x  = subGrepn xs cmp (n + 1) (n:nxs)
    | otherwise = subGrepn xs cmp (n + 1) nxs

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

isIn :: (Eq a) => a -> [a] -> Bool
isIn _ [] = False
isIn cmp (x:xs)
    | cmp == x = True
    | otherwise = isIn cmp xs

isInn :: (Eq a) => [a] -> [a] -> Bool
isInn [] _ = True
isInn (cmp:valxs) xs
    | (isIn cmp xs) = isInn valxs xs
    | otherwise   = False

closerIdx :: (Num a, Ord a) => a -> [a] -> Int
closerIdx cmp xs = myMinIdx2 (subCloserIdx cmp xs)

subCloserIdx :: (Num a, Ord a) => a -> [a] -> [a]
subCloserIdx _ [] = []
subCloserIdx cmp (x:xs) = (abs(cmp - x)):(subCloserIdx cmp xs)

closerIdxSpe :: (Num a, Ord a) => a -> [(Tree a Char)] -> Int
closerIdxSpe cmp xs = myMinIdx2 (subCloserIdxSpe cmp xs)

subCloserIdxSpe :: (Num a, Ord a) => a -> [(Tree a Char)] -> [a]
subCloserIdxSpe _ [] = []
subCloserIdxSpe cmp ((Node x _ _):xs) = (abs(cmp - x)):(subCloserIdxSpe cmp xs)

getMins :: [Int] -> (Int, Int)
getMins xs
    | length xs == 2 = ((xs !! 0), (xs !! 1))
    | length xs < 2  = ((xs !! 0), -1)
    | otherwise      = (-1, -1)

--sortWith :: (Ord a, Eq a) => [a] -> [b] -> ([a], [b])
--sortWith xs xs2 = 
--    let newxs = quickSortAsc xs
--        ids = grepn

myTrees :: [Tree Int Char]
myTrees = [
          Node 34  (Leaf 'A') (Leaf 'A'), 
          Node 121 (Leaf 'A') (Leaf 'A'),
          Node 21  (Leaf 'A') (Leaf 'A'),
          Node 12  (Leaf 'A') (Leaf 'A'),
          Node 65  (Leaf 'A') (Leaf 'A'),
          Node 6   (Leaf 'A') (Leaf 'A')
          ]


-- 1
-- Find the last element of a list.

myLast :: [a] -> a
myLast xs = last xs

-- 1 other version

myButLast :: [a] -> a
myButLast (x1:x2:x3:xs) = x3

myButLast2 :: [a] -> a
myButLast2 xs
    | length xs > 1 = (xs !! (length xs - 2))
    | otherwise = head xs


--3
-- Find the K'th element of a list. 

elemAt :: [a] -> Int -> Maybe a
elemAt xs idx
    | idx > length xs = Nothing
    | otherwise = Just (xs !! (idx - 1))


--4
-- Find the number of elements in a list. 
myLength :: [a] -> Int
myLength xs = length xs


--5
-- Reverse a list. 
myReverse :: [a] -> [a]
myReverse xs = reverse xs

myReverse2 :: [a] -> [a]
myReverse2 [] = []
myReverse2 xs = (last xs):(myReverse2 (init xs))

--6
-- Find out whether a list is a palindrome. 

getRangeList :: [a] -> [Int] -> [a]
getRangeList [] _ = []
getRangeList _ [] = []
getRangeList xs (idx:ids) = (xs !! idx):(getRangeList xs ids) 

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [a] = True
isPalindrome xs = 
    let l = if (length xs) `mod` 2 == 0
            then (length xs) `div` 2 - 1
            else ((length xs) - 1) `div` 2
        xs1 = getRangeList xs [0..l]
        extr = (length xs - 1 - l)
        xs2 = getRangeList xs (reverse ([extr..(length xs - 1)]))
    in xs1 == xs2


--7
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively). 
--Example in Haskell:
-- We have to define a new data type, because lists in Haskell are homogeneous.

data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat (fmap flatten xs)

--8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

-- Example: 
-- λ> compress "aaaabccaadeeee"
-- "abcade"

compress :: [Char] -> [Char]
compress [] = []
compress xs = 
    let cmp = head xs
        newxs = subCompress xs cmp
    in cmp:(compress newxs)
    
subCompress :: [Char] -> Char -> [Char]
subCompress [] _ = []
subCompress (x:xs) cmp
    | x == cmp = subCompress xs cmp
    | otherwise = x:xs


--9
-- Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists. 
--Example in Haskell:

-- λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
--             'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: [Char] -> [[Char]]
pack [] = []
pack xs = 
    let cmp = head xs
        (newxs, xs2) = subPack (xs, []) cmp
    in xs2:(pack newxs)
    
subPack :: ([Char], [Char]) -> Char -> ([Char], [Char])
subPack ([], xs2) _ = ([], xs2)
subPack ((x:xs), xs2) cmp
    | x == cmp = subPack (xs, (cmp:xs2)) cmp
    | otherwise = ((x:xs), xs2)


--10
-- Run-length encoding of a list. 
-- Use the result of Problem 9 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

-- Example in Haskell:

-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode :: [Char] -> [(Int, Char)]
encode [] = []
encode xs = 
    let cmp = head xs
        (newxs, nb) = subEncode xs 0 cmp
    in (nb, cmp):(encode newxs)
    
subEncode :: [Char] -> Int -> Char -> ([Char], Int)
subEncode [] nb _ = ([], nb)
subEncode (x:xs) nb cmp
    | x == cmp = subEncode xs (nb + 1) cmp
    | otherwise = ((x:xs), nb)

--11 (foreshadowing P.13)
-- Modified run-length encoding. 
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

-- Example in Haskell:

-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
-- Multiple 2 'a',Single 'd',Multiple 4 'e']

data Occ a b = Single b | Multiple a b deriving (Show)

encodeModified :: [Char] -> [(Occ Int Char)]
encodeModified [] = []
encodeModified xs = 
    let cmp = head xs
        (newxs, occ) = subEncodeModified xs 0 cmp
    in occ:(encodeModified newxs)
    
subEncodeModified :: [Char] -> Int -> Char -> ([Char], (Occ Int Char))
subEncodeModified [] nb cmp = ([], if nb == 1
                                 then Single cmp
                                 else (Multiple nb cmp))
subEncodeModified (x:xs) nb cmp
    | x == cmp = subEncodeModified xs (nb + 1) cmp
    | otherwise = ((x:xs), if nb == 1
                           then Single cmp
                           else (Multiple nb cmp))

--12
-- Decode a run-length encoded list. 
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

-- Example in Haskell:

-- λ> decodeModified 
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

decodeModified :: [(Occ Int Char)] -> [Char]
decodeModified [] = []
decodeModified ((Multiple hmn chr):xs) = (subDecodeModified chr hmn) ++ (decodeModified xs)
decodeModified ((Single chr):xs) = chr:(decodeModified xs)

subDecodeModified :: Char -> Int -> [Char]
subDecodeModified chr 0 = []
subDecodeModified chr hmn = chr:(subDecodeModified chr (hmn - 1))

--13

--already in 11 lol

-- 14

--  Duplicate the elements of a list. 
-- Example in Haskell:
-- 
-- λ> dupli [1, 2, 3]
-- [1,1,2,2,3,3]


duppli :: [a] -> [a]
duppli [] = []
duppli (x:xs) = x:x:(duppli xs)

-- 15
-- Replicate the elements of a list a given number of times. 

-- Example in Haskell:
-- 
-- λ> repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) hmn = (subRepli x hmn) ++ (repli xs hmn)

subRepli :: a -> Int -> [a]
subRepli _ 0 = []
subRepli x hmn = x:(subRepli x (hmn - 1))

-- 16
-- Drop every N'th element from a list. 
-- Example in Haskell:
-- 
-- λ> dropEvery "abcdefghik" 3
-- "abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery xs idx = subDropEvery xs idx 1

subDropEvery :: [a] -> Int -> Int -> [a]
subDropEvery [] _ _ = []
subDropEvery (x:xs) idx idx2
    | idx /= idx2 = x:(subDropEvery xs idx (idx2 + 1))
    | otherwise = subDropEvery xs idx 1


-- 17 one of the hardest i find
-- Split a list into two parts; the length of the first part is given. 
-- Do not use any predefined predicates. 
-- Example in Haskell:
-- 
-- λ> split "abcdefghik" 3
-- ("abc", "defghik")

split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) n =
    let (xs1, xs2) = split xs (n - 1)
    in (x:xs1, xs2) -- the x:xs will be applied just before return

-- 18 broooo, that is alredy done with getRangeList

-- Extract a slice from a list. 
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1. 
-- Example in Haskell:
-- 
-- λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

-- 19
-- Rotate a list N places to the left. 
-- Hint: Use the predefined functions length and (++). 
-- Examples in Haskell:
-- 
-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- 
-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"

rotate :: [a] -> Int -> [a]
rotate xs rt = if rt > 0
               then subPosRotate xs rt
               else subNegRotate xs rt

subPosRotate :: [a] -> Int -> [a]
subPosRotate xs 0 = xs
subPosRotate (x:xs) rt = subPosRotate (xs ++ [x]) (rt - 1)

subNegRotate :: [a] -> Int -> [a]
subNegRotate xs 0 = xs
subNegRotate xs rt = 
    let x = last xs
        newxs = x:(init xs)
    in subNegRotate newxs (rt + 1)

-- 20

-- Remove the K'th element from a list. 
-- (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)
-- 
-- Example in Haskell:
-- 
-- λ> removeAt 2 "abcd"
-- ('b',"acd")

removeAt :: Int -> [a] -> (a, [a])
removeAt idx xs = ((xs !! (idx - 1)), subRemoveAt idx 1 xs)

subRemoveAt :: Int -> Int -> [a] -> [a]
subRemoveAt _ _ [] = []
subRemoveAt idx curidx (x:xs)
    | idx /= curidx = x:(subRemoveAt idx (curidx + 1) xs)
    | otherwise = subRemoveAt idx (curidx + 1) xs



-- 21

-- Insert an element at a given position into a list. 

-- Example in Haskell:
-- 
-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt insrt xs idx = 
    let (xs1, xs2) = split xs (idx - 1)
    in xs1 ++ [insrt] ++ xs2

-- 22
-- Create a list containing all integers within a given range. 
-- Example in Haskell:
-- 
-- λ> range 4 9
-- [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range x1 x2
    | x1 < x2 = x1:(range (x1 + 1) x2)
    | otherwise = x1:[]

--23
-- Extract a given number of randomly selected elements from a list. 
-- Example in Haskell:
-- 
-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda

rnd_select :: [a] -> Int -> [a]
rnd_select xs n = subRnd_Select xs n 0 (R.mkStdGen $ length xs) (length xs)

subRnd_Select :: [a] -> Int -> Int -> R.StdGen -> Int -> [a]
subRnd_Select xs n n2 gen l
    | n2 < n =
        let (val, newgen) = R.random gen
        in (xs !! ((abs val) `mod` l)):(subRnd_Select xs n (n2 + 1) newgen l)
    | otherwise = []


-- 24
-- Lotto: Draw N different random numbers from the set 1..M. 
-- Example in Haskell:
-- 
-- λ> diff_select 6 49
-- [23,1,17,33,21,37]

diff_select :: Int -> Int -> [Int]
diff_select n max = subDiff_Select n xs (R.mkStdGen l) 0 [] l
    where xs = range 1 max
          l = length xs

subDiff_Select :: Int -> [Int] -> R.StdGen -> Int -> [Int] -> Int -> [Int]
subDiff_Select n xs gen n2 xs2 l
    | n2 < n = 
        let (val, newgen) = R.random gen
            idx = val `mod` l
        in if (idx + 1) `elem` xs2
           then subDiff_Select n xs newgen n2 xs2 l
           else subDiff_Select n xs newgen (n2 + 1) ((idx + 1):xs2) l
    | otherwise = xs2


-- 25
-- Generate a random permutation of the elements of a list. 
-- Example in Haskell:
-- 
-- λ> rnd_permu "abcdef"
-- "badcef"

rnd_permu :: (Eq a) => [a] -> [a]
rnd_permu xs = subRnd_Permu l xs (R.mkStdGen l) 0 []
    where l = length xs

subRnd_Permu :: (Eq a) => Int -> [a] -> R.StdGen -> Int -> [a] -> [a]
subRnd_Permu l xs gen n2 xs2
    | n2 < l = 
        let (val, newgen) = R.random gen
            idx = val `mod` l
            myval = (xs !! idx)
        in if myval `elem` xs2
           then subRnd_Permu l xs newgen n2 xs2
           else subRnd_Permu l xs newgen (n2 + 1) (myval:xs2)
    | otherwise = xs2


-- 26

-- Generate combinations of K distinct objects chosen from the N elements of a list. 
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list. 
-- Example in Haskell:
-- 
-- λ> combinations 3 "abcdef"
-- ["abc","abd","abe",...]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k - 1) xs) ++ combinations k xs


-- 27 
-- Group the elements of a set into disjoint subsets. 

-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
-- 
-- You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
-- 
-- Example in Haskell:
-- 
-- λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)
-- 
-- λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)

-- Wrong, but looks appealing at first since it is telled that [(A, B), (B, A)] are not the same...

--group2 :: (Eq a) => [Int] -> [a] -> [[[a]]]
--group2 nxs xs = 
--    let [xs1, xs2, xs3] = subGroup xs nxs
--    in [(a, b, c) | a <- xs1, b <- xs2, c <- xs3]
--
--subGroup :: (Eq a) => [a] -> [Int] -> [[[a]]]
--subGroup _ [] =  []
--subGroup xs (n:nxs) = (combinations n xs):(subGroup xs nxs)

rmFirstOccs :: (Eq a) => [a] -> [a] -> [a]
rmFirstOccs [] _ = []
rmFirstOccs xs [] = xs
rmFirstOccs xs1 (x2:xs2) = 
    let newxs = mayRm xs1 x2 False
    in rmFirstOccs newxs xs2

mayRm :: (Eq a) => [a] -> a -> Bool -> [a]
mayRm [] _ _ = []
mayRm (x1:xs) x alrd
    | x1 /= x = x1:(mayRm xs x alrd)
    | otherwise = if alrd
                  then x1:(mayRm xs x alrd)
                  else mayRm xs x True

group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x:xs) = 
    let (subxs, newxs) = subGroup (x:xs) x []
    in subxs:(group newxs)

subGroup :: (Eq a) => [a] -> a -> [a] -> ([a], [a])
subGroup [] _ xs2 = (xs2, [])
subGroup (x:xs) cmp xs2
    | x == cmp = subGroup xs cmp (cmp:xs2)
    | otherwise = (xs2, (x:xs)) 

nGroup :: (Eq a) => [a] -> [Int] -> [[[a]]]
nGroup [] [] = [[]]
nGroup xs (n:ns) =
  [g : gs | g  <- combinations n xs, gs <- nGroup (xs `rmFirstOccs` g) ns]

-- This is actually very smart, recursively eliminating already taken elements with custom rmFirstOccs, a little bit like combinations


-- 28.a

-- Sorting a list of lists according to length of sublists. 

-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa. 

-- Example in Haskell:
-- 
-- λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = 
    let lowerxs = lsort [a | a <- xs, length a <= l]
        higherxs = lsort [a | a <- xs, length a > l]
    in lowerxs ++ [x] ++ higherxs
    where l = length x

--based on that quickSort impl

quickSortAsc :: (Eq a, Ord a) => [a] -> [a]
quickSortAsc [] = []
quickSortAsc (x:xs) = 
    let lowerxs = quickSortAsc [a | a <- xs, a <= x]
        higherxs = quickSortAsc [a | a <- xs, a > x]
    in lowerxs ++ [x] ++ higherxs

quickSortDesc :: (Eq a, Ord a) => [a] -> [a]
quickSortDesc [] = []
quickSortDesc (x:xs) = 
    let lowerxs = quickSortDesc [a | a <- xs, a <= x]
        higherxs = quickSortDesc [a | a <- xs, a > x]
    in higherxs ++ [x] ++ lowerxs

--28.b

-- b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later. 

-- Example in Haskell:
-- 
-- λ> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]

fsort :: [[a]] -> [[a]]
fsort [] = []
fsort (x:xs) = 
    let lowerxs  = fsort [a | a <- xs, 
                            (subFsort (x:xs) (length a) 0) <= f]
        higherxs = fsort [a | a <- xs, 
                            (subFsort (x:xs) (length a) 0) > f]
    in lowerxs ++ [x] ++ higherxs
    where f = subFsort xs (length x) 1

subFsort :: [[a]] -> Int -> Int -> Int
subFsort [] _ n = n
subFsort (x:xs) cmp n
    | length x == cmp = subFsort xs cmp (n + 1)
    | otherwise = subFsort xs cmp n


-- 31

-- Determine whether a given integer number is prime. 

-- Example in Haskell:
-- 
-- λ> isPrime 7
-- True

isPrime :: Int -> Bool
isPrime x = subIsPrime x 2

subIsPrime :: Int -> Int -> Bool
subIsPrime x n 
    | x > n = if x `mod` n == 0
              then False
              else subIsPrime x (n + 1)
    | otherwise = if x /= 2
                  then True
                  else False

-- 32

-- Determine the greatest common divisor of two positive integer numbers. 

-- Example in Haskell:
-- 
-- λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]

-- Hint: Euclide's algo

myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

-- 33

-- Determine whether two positive integer numbers are coprime. 
-- Two numbers are coprime if their greatest common divisor equals 1. 

-- Example in Haskell:
-- 
-- λ> coprime 35 64
-- True

coprime :: Int -> Int -> Bool
coprime a b = (myGCD a b) == 1

-- 34

-- Calculate Euler's totient function phi(m). 

-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
-- 
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1. 

-- Example in Haskell:
-- 
-- λ> totient 10
-- 4

totient :: Int -> Int
totient x = subTotient x x 1 

subTotient :: Int -> Int -> Int -> Int
subTotient 1 cmp n = n
subTotient x cmp n
    | coprime x cmp = subTotient (x - 1) cmp (n + 1)
    | otherwise = subTotient (x - 1) cmp n


-- 35

-- Determine the prime factors of a given positive integer. 

-- Construct a flat list containing the prime factors in ascending order. 
-- Example in Haskell:
-- 
-- λ> primeFactors 315
-- [3, 3, 5, 7]

primeFactors :: Int -> [Int]
primeFactors x = 
    let n = subPrimeFactor x 2
    in if n /= x
       then n:(primeFactors (x `div` n))
       else n:[] 

subPrimeFactor :: Int -> Int -> Int
subPrimeFactor x n
    | n == x = x
    | x `mod` n /= 0 = subPrimeFactor x (n + 1)
    | otherwise = if isPrime n
                  then n
                  else subPrimeFactor x (n + 1)

-- 36

-- Determine the prime factors and their multiplicities of a given positive integer. 
-- Construct a list containing each prime factor and its multiplicity. 

-- Example in Haskell:
-- 
-- λ> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult x = 
    let ((f, x2), n) = subPrimeFactorMult x 2
    in if n /= x && n /= 1
       then (n, f):(primeFactorsMult x2)
       else ((n, f):[])

subPrimeFactorMult :: Int -> Int -> ((Int, Int), Int)
subPrimeFactorMult x n
    | n == x = ((1, 1), x)
    | x `mod` n /= 0 = subPrimeFactorMult x (n + 1)
    | otherwise = if isPrime n
                  then (sub2PrimeFactorMult n (x `div` n) 1, n)
                  else subPrimeFactorMult x (n + 1)

sub2PrimeFactorMult :: Int -> Int -> Int -> (Int, Int)
sub2PrimeFactorMult n x f
    | x `mod` n == 0 = sub2PrimeFactorMult n (x `div` n) (f + 1)
    | otherwise = (f, x)


-- 37

-- Calculate Euler's totient function phi(m) (improved). 
-- See Problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
-- 
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
--          (p2 - 1) * p2 ** (m2 - 1) * 
--          (p3 - 1) * p3 ** (m3 - 1) * ...
-- 
-- Note that a ** b stands for the b'th power of a. 

totient2 :: Int -> Int
totient2 x = mult [(p - 1) * p ^ (m -1) | (p, m) <- primeFactorsMult x]

mult :: (Num a) => [a] -> a
mult [] = 1
mult (x:xs) = x * (mult xs)


-- 38 

-- Compare the two methods of calculating Euler's totient function. 
-- Use the solutions of Problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example. 
-- do it on your own


-- 39

--  A list of prime numbers in a given range.

-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
-- 
-- Example in Haskell:
-- 
-- λ> primesR 10 20
-- [11,13,17,19]

primesR :: Int -> Int -> [Int]
primesR a b
    | a /= b = if isPrime a
               then (a:(primesR (a + 1) b))
               else primesR (a + 1) b
    | otherwise = []

-- 40

-- Goldbach's conjecture. 


-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.

-- Example in Haskell:
-- 
-- λ> goldbach 28
-- (5, 23)

goldbach :: Int -> (Int, Int)
goldbach x = subGoldbach x 1 (subGoldbachDesc x)

subGoldbach :: Int -> Int -> Int -> (Int, Int)
subGoldbach x v1 v2 = 
    let (newv1, newv2) = updateInterval v1 v2
    in if (v1 + v2) == x
       then (v1, v2)
       else subGoldbach x newv1 newv2

updateInterval :: Int -> Int -> (Int, Int)
updateInterval v1 v2
    | v1 == v2 = (1, subGoldbachDesc v2)
    | otherwise = (subGoldbachAsc (v1 + 1) v2, v2)

subGoldbachDesc :: Int -> Int
subGoldbachDesc 1 = 1
subGoldbachDesc x
    | isPrime x = x
    | otherwise = subGoldbachDesc (x - 1)

subGoldbachAsc :: Int -> Int -> Int
subGoldbachAsc x cmp
    | x == cmp = cmp
    | isPrime x = x
    | otherwise = subGoldbachAsc (x + 1) cmp


--41

-- A list of even numbers and their Goldbach compositions in a given range. 

-- Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
--
-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000. 

-- Example in Haskell:
-- 
-- λ> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- λ> goldbachList' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b =
    let a2 = if a `mod` 2 == 0
            then a
            else a + 1
    in subGoldbachList a2 b

subGoldbachList :: Int -> Int -> [(Int, Int)]
subGoldbachList a b
    | a < b = (goldbach a):(subGoldbachList (a + 2) b)
    | otherwise = []

--46
-- Truth tables for logical expressions. 

-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
-- 
-- A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
-- 
-- Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables. 

-- Example in Haskell:
-- 
-- λ> table (\a b -> (and' a (or' a b)))
-- True True True
-- True False True
-- False True False
-- False False False

myOr :: Bool -> Bool -> Bool
myOr a b
    | a = True
    | b = True
    | otherwise = False

myOrn :: [Bool] -> Bool
myOrn [] = False
myOrn (x:xs)
    | x = True
    | otherwise = myOrn xs

myAnd :: Bool -> Bool -> Bool
myAnd a b
    | a = b
    | b = a
    | otherwise = False

myAndn :: [Bool] -> Bool
myAndn [] = True
myAndn (x:xs)
    | x = myAndn xs
    | otherwise = False

myNor :: Bool -> Bool -> Bool
myNor a b = not $ myOr a b

myXor :: Bool -> Bool -> Bool
myXor a b
    | a /= b = True
    | otherwise = False

myImpl :: Bool -> Bool -> Bool
myImpl a b
    | a = b
    | otherwise = b

myXand :: Bool -> Bool -> Bool
myXand a b
    | a /= b = False
    | otherwise = True

myEqu :: Bool -> Bool -> Bool
myEqu a b = myXand a b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ print [(a, b, f a b) | a <- [True, False], b <- [True, False]]

-- 47

-- Truth tables for logical expressions (part 2). 
-- Continue Problem 46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java. 

-- Example in Haskell:

-- λ> table2 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False

--Already done

-- 48

-- Truth tables for logical expressions (part 3).
--Generalize Problem 47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List. 

-- Example in Haskell:
-- 
-- λ> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- -- infixl 3 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True
-- 
-- -- infixl 7 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False False
-- False True  True  False
-- False True  False False
-- False False True  False
-- False False False False

myReplicate :: Int -> a -> [a]
myReplicate 0 x = []
myReplicate n x = x:(myReplicate (n - 1) x)

myReplicateM :: Int -> [a] -> [[a]]
myReplicateM n xs = 
    let newxs = myReplicate n xs
    in mySequence newxs

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ print [unwords (map showspe val) ++ " => " ++ show (f val) | val <- myReplicateM n [True, False]]
    where showspe x = if x
                      then "True "
                      else "False"

--48 bis, (not existing but i want to create my own sequence)

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


-- 49
-- Gray codes. 
-- An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
-- 
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
-- 
-- Find out the construction rules and write a predicate with the following specification:
-- 
-- % gray(N,C) :- C is the N-bit Gray code
-- 
-- Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?
-- 
-- Example in Haskell:
-- 
-- λ> gray 3
-- ["000","001","011","010","110","111","101","100"]

gray :: Int -> [[Char]]
gray n = 
    let xs = myReplicate n "10"
    in mySequence xs

-- 50

-- Huffman codes. 

-- We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2 defined as follows:
-- 
-- % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
-- 
-- Example in Haskell:
-- 
-- λ> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b) deriving (Show)

huffmanTree :: [(Char, Int)] -> [(Char, [Char])]
huffmanTree xs = 
    let (fs, hs)  = subHuffmanTreePrepare xs [] []
        ts = subHuffmanTree1 fs hs []
        newts = subHuffmanTree2 ts
    in subHuffmanTree3 newts []

subHuffmanTreePrepare :: [(Char, Int)] -> [Int] -> [Char] 
                                -> ([Int], [Char])
subHuffmanTreePrepare [] fs hs = (fs, hs)
subHuffmanTreePrepare (x:xs) fs hs = 
    let f   = snd x
        val = fst x
    in subHuffmanTreePrepare xs (f:fs) (val:hs)

subHuffmanTree1 :: [Int] -> [Char] -> [(Tree Int Char)] 
                        -> [(Tree Int Char)]
subHuffmanTree1 [] _  ts = ts
subHuffmanTree1 fs hs ts =
    let minxs = myMinN2 fs 2
        (min1, min2) = getMins minxs
        (newfs3, newhs3, newts3) = if (min1 /= -1) `myAnd` (min2 /= -1)
                                   then let (newfs, newhs, newts) = subHuffmanTreePair fs hs ts [min1, min2]
                                            ids1 = grep2 min1 newfs
                                            ids2 = grep2 min2 newfs
                                            valxs = filter (\(x, y) -> x /= (-1)) [(ids1, min1), (ids2, min2)]
                                            (newfs2, newhs2, newts2) = subHuffmanTreeSingle newfs newhs newts valxs
                                        in (newfs2, newhs2, newts2)
                                   else let ids1 = grep2 min1 fs
                                            ids2 = grep2 min2 fs
                                            valxs = filter (\(x, y) -> x /= (-1)) [(ids1, min1), (ids2, min2)]
                                            (newfs2, newhs2, newts2) = subHuffmanTreeSingle fs hs ts valxs
                                        in (newfs2, newhs2, newts2)
    in subHuffmanTree1 newfs3 newhs3 newts3

subHuffmanTreePair :: [Int] -> [Char] -> 
                      [(Tree Int Char)] -> [Int] ->
                      ([Int], [Char], [(Tree Int Char)])
subHuffmanTreePair fs hs ts minxs 
    | length minxs == 2 = 
        let [(min2, idx2), (min1, idx1)] = myMinIdxN fs 2
            chr1 = (hs !! idx1)
            chr2 = (hs !! idx2)
            ftree = if min1 >= min2
                    then Node (min1 + min2) (Leaf chr2) (Leaf chr1)
                    else Node (min1 + min2) (Leaf chr1) (Leaf chr2)
            newts = ts ++ [ftree]
            (newfs, newhs) = if idx2 > idx1
                             then (deleteListElemn fs [idx1, idx2], 
                                   deleteListElemn hs [idx1, idx2])
                             else (deleteListElemn fs [idx2, idx1], 
                                   deleteListElemn hs [idx2, idx1])
            newminxs = myMinN2 newfs 2
        in  subHuffmanTreePair newfs newhs newts newminxs
    | otherwise = (fs, hs, ts)

subHuffmanTreeSingle :: [Int] -> [Char] ->
                        [(Tree Int Char)] -> [(Int, Int)] -> 
                        ([Int], [Char], [(Tree Int Char)])
subHuffmanTreeSingle fs hs ts [] = (fs, hs, ts)
subHuffmanTreeSingle fs hs ts [(_, minx)] = 
    let idx = closerIdxSpe minx ts
        (Node sm l r) = (ts !! idx)
        chrvl = (hs !! idx)
        newtree = if minx >= sm
                  then (Node (sm + minx) (Node sm l r) (Leaf chrvl))
                  else (Node (sm + minx) (Leaf chrvl) (Node sm l r))
        newts = updateListElem ts idx newtree
        newfs = deleteListElem fs idx
        newhs = deleteListElem hs idx
    in (newfs, newhs, newts)
        
subHuffmanTree2 :: [(Tree Int Char)] -> (Tree Int Char)
subHuffmanTree2 [x] = x
subHuffmanTree2 ((Node x1 l1 r1):xs) = 
    let idx = closerIdxSpe x1 xs
        (Node x2 l2 r2) = (xs !! idx)
        tree = if x1 >= x2
               then Node (x1 + x2) (Node x2 l2 r2) (Node x1 l1 r1)
               else Node (x1 + x2) (Node x1 l1 r1) (Node x2 l2 r2)
        newxs = updateListElem xs idx tree
    in subHuffmanTree2 newxs

subHuffmanTree3 :: (Tree Int Char) -> [Char] -> [(Char, [Char])]
subHuffmanTree3 (Leaf chr) hs = [(chr, hs)]
subHuffmanTree3 (Node _ l r) hs = subHuffmanTree3 l (hs ++ ['0']) ++ subHuffmanTree3 r (hs ++ ['1'])


-- 55 prev

--Count how many nodes a full tree has given its depth

countNodesBinary :: Int -> Int
countNodesBinary 0 = 1
countNodesBinary n = 2 ^ n + (countNodesBinary (n - 1))

-- 55 all permutations from 1 Node = (n - 1) permutations where n is the dept level

data MyTree a = MyEmpty | MyNode a (MyTree a) (MyTree a) deriving (Show, Eq)

cbalTree1 :: Int -> [MyTree Char]
cbalTree1 n = subCbalTree1 (n - 1) (MyNode 'x' (MyEmpty) (MyEmpty))

subCbalTree1 :: Int -> (MyTree Char) -> [MyTree Char]
subCbalTree1 0 tree = [tree]
subCbalTree1 n tree = (subCbalTree1 (n - 1) (MyNode 'X' tree (MyEmpty)))
                      ++ (subCbalTree1 (n - 1) (MyNode 'X' (MyEmpty) tree))

-- 55 Wow this is elegant !!!!!

-- Construct completely balanced binary trees. 

-- In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
-- 
-- Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree. 

-- Example in Haskell, whitespace and "comment diagrams" added for clarity and exposition:
-- 
-- λ> cbalTree 4
-- [
-- -- permutation 1
-- --     x
-- --    / \
-- --   x   x
-- --        \
-- --         x
-- Branch 'x' (Branch 'x' Empty Empty) 
--            (Branch 'x' Empty 
--                        (Branch 'x' Empty Empty)),
-- 
-- -- permutation 2
-- --     x
-- --    / \
-- --   x   x
-- --      /
-- --     x
-- Branch 'x' (Branch 'x' Empty Empty) 
--            (Branch 'x' (Branch 'x' Empty Empty) 
--                        Empty),
-- 
-- -- permutation 3
-- --     x
-- --    / \
-- --   x   x
-- --    \
-- --     x
-- Branch 'x' (Branch 'x' Empty 
--                        (Branch 'x' Empty Empty)) 
--            (Branch 'x' Empty Empty),
-- 
-- -- permutation 4
-- --     x
-- --    / \
-- --   x   x
-- --  /
-- -- x
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
--                        Empty) 
--            (Branch 'x' Empty Empty)
-- ]

cbalTree :: Int -> [(MyTree Char)]
cbalTree 0 = [MyEmpty]
cbalTree 1 = [MyNode 'x' MyEmpty MyEmpty]
cbalTree n = if n `mod` 2 == 1 then 
             [ MyNode 'x' l r | l <- cbalTree ((n - 1) `div` 2), 
                                r <- cbalTree ((n - 1) `div` 2) ] 
             else 
             concat [ [MyNode 'x' l r, MyNode 'x' r l] | l <- cbalTree ((n - 1) `div` 2), 
                                                         r <- cbalTree (n `div` 2) ]


--[MyNode 'x' (MyNode 'x' (MyNode 'x' (MyNode 'x' MyEmpty MyEmpty) (MyNode 'x' MyEmpty MyEmpty)) (MyNode 'x' (MyNode 'x' MyEmpty MyEmpty) (MyNode 'x' MyEmpty MyEmpty))) (MyNode 'x' (MyNode 'x' (MyNode 'x' MyEmpty MyEmpty) (MyNode 'x' MyEmpty MyEmpty)) (MyNode 'x' (MyNode 'x' MyEmpty MyEmpty) (MyNode 'x' MyEmpty MyEmpty)))]
--
--MyNode 'x' (MyNode 'x' (MyNode 'x' (MyNode 'x' MyEmpty MyEmpty) (MyNode 'x' MyEmpty MyEmpty)) (MyNode 'x' (MyNode 'x' MyEmpty MyEmpty) MyEmpty)) (MyNode 'x' (MyNode 'x' (MyNode 'x' MyEmpty MyEmpty) MyEmpty) (MyNode 'x' (MyNode 'x' MyEmpty MyEmpty) MyEmpty))

-- 56

-- Symmetric binary trees. 
-- Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
-- 
-- Example in Haskell:
-- 
-- λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
-- False
-- λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
-- True

isSymetric :: (Eq a) => (MyTree a) -> Bool
isSymetric (MyNode _ l r) = l == r

-- 57
-- Binary search trees. 
-- Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers. 

-- Example:
-- 
-- * construct([3,2,5,7,1],T).
-- T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
-- 
-- Then use this predicate to test the solution of Problem 56.
-- 
-- Example:
-- 
-- * test-symmetric([5,3,18,1,4,12,21]).
-- Yes
-- * test-symmetric([3,2,5,7,4]).
-- No
-- 
-- Example in Haskell:
-- 
-- λ> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
-- λ> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
-- True
-- λ> symmetric . construct $ [3, 2, 5, 7, 1]
-- True

construct :: (Ord a) => [a] -> (MyTree a)
construct (x:xs) = subConstruct xs (MyNode x MyEmpty MyEmpty)

subConstruct :: (Ord a) => [a] -> (MyTree a) -> (MyTree a)
subConstruct [] _ = MyEmpty
subConstruct xs (MyNode cmp _ _) = 
    let xs1 = filter (< cmp) xs
        x1 = myMax xs1
        xs2 = filter (> cmp) xs
        x2 = myMin xs2
    in MyNode cmp (subConstruct xs1 (MyNode x1 MyEmpty MyEmpty)) (subConstruct xs2 (MyNode x2 MyEmpty MyEmpty))


-- 58

-- Generate-and-test paradigm. 

-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes. 

-- Example in Haskell:
-- 
-- λ> symCbalTrees 5
-- [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]

--symCbalTree :: Int -> [(MyTree Char)]
--symCbalTree n = keepSymetric (subSymCbalTree (n - 1) (MyNode 'X' MyEmpty MyEmpty))
--
--subSymCbalTree :: Int -> (MyTree Char) -> [(MyTree Char)]
--subSymCbalTree 0 tree = [tree]
--subSymCbalTree 1 tree = (subSymCbalTree 0 (MyNode 'X' tree MyEmpty)) ++ (subSymCbalTree 0 (MyNode 'X' MyEmpty tree))
--subSymCbalTree n tree = (subSymCbalTree (n - 2) (MyNode 'X' tree (MyNode 'X' MyEmpty MyEmpty))) ++ (subSymCbalTree (n - 2) (MyNode 'X' (MyNode 'X' MyEmpty MyEmpty) tree))

-- Indeed very good because if nodes is even, then subnodes are odds then no symetry can be prooved between the right and left part of the tree with even number of nodes, because one side will hae more nodes than the other
-- So this just takes the quotient of number of nodes, computes all posible trees with this even quotient (odd - 1 == even, where odd is the number of nodes of the general tree)
-- this is done for one side (in this case the roght), so the left must be symetric to the right, how to do that???
-- just 'reverse' all the nodes computed from the cbalTree function for this iteration
-- We iterate, with the comprehension list to solve all the possible trees to do with ((n - 1) / 2) nodes and apply the same logic.... ;)

symCbalTree :: Int -> [(MyTree Char)]
symCbalTree n = if n `mod` 2 == 0
                then []
                else [MyNode 'X' t (reverseTree t) | t <- cbalTree (n `div` 2)]

reverseTree :: (MyTree Char) -> (MyTree Char)
reverseTree MyEmpty = MyEmpty
reverseTree (MyNode x l r) = MyNode x (reverseTree r) (reverseTree l)


-- 59, not height balanced... 

-- Construct height-balanced binary trees. 

-- In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
-- 
-- Construct a list of all height-balanced binary trees with the given element and the given maximum height. 

-- Example in Haskell:
-- 
-- λ> take 4 $ hbalTree 'x' 3
-- [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]

nhbalTree :: a -> Int -> [(MyTree a)]
nhbalTree val n = subNHbalTree val (n - 1) (MyNode val MyEmpty MyEmpty)

subNHbalTree :: a -> Int -> (MyTree a) -> [(MyTree a)]
subNHbalTree _ 0 tree = [tree]
subNHbalTree val n tree = (subNHbalTree val (n - 1) (MyNode val tree MyEmpty)) ++ 
                         (subNHbalTree val (n - 1) (MyNode val MyEmpty tree))

-- 59

hbalTree :: a -> Int -> [(MyTree a)]
hbalTree val n = subHbalTree val (n - 1) 0 (MyNode val MyEmpty MyEmpty)

subHbalTree :: a -> Int -> Int -> (MyTree a) -> [(MyTree a)]
subHbalTree _ 0 _ tree = [tree]
subHbalTree val n cmp tree 
    | cmp > 0 = (subHbalTree val (n - 1) (cmp + 1) (MyNode val tree (restTree val cmp))) ++ 
                         (subHbalTree val (n - 1) (cmp + 1) (MyNode val (restTree val cmp) tree))
    | otherwise = (subHbalTree val (n - 1) (cmp + 1) (MyNode val tree MyEmpty)) ++ 
                         (subHbalTree val (n - 1) (cmp + 1) (MyNode val MyEmpty tree))
    where restTree _ 0   = MyEmpty
          restTree val n = MyNode val (restTree val (n - 1)) (restTree val (n - 1)) 

--hbalTree2 x = map fst . hbalTree'
--    where hbalTree' 0 = [(MyEmpty, 0)]
--          hbalTree' 1 = [(MyNode x MyEmpty MyEmpty, 1)]
--          hbalTree' n =
--                let t = hbalTree' (n-2) ++ hbalTree' (n-1)
--                in  [(MyNode x lb rb, h) | (lb,lh) <- t, (rb,rh) <- t
--                                         , let h = 1 + max lh rh, h == n]

hbalTree2 :: a -> Int -> [MyTree a]
hbalTree2 x 0 = [MyEmpty]
hbalTree2 x 1 = [MyNode x MyEmpty MyEmpty]
hbalTree2 x h = [MyNode x l r |
        (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
        l <- hbalTree2 x hl, r <- hbalTree2 x hr]

--MyNode 'X' (MyNode 'X' MyEmpty (MyNode 'X' MyEmpty MyEmpty)) (MyNode 'X' (MyNode 'X' MyEmpty MyEmpty) (MyNode 'X' MyEmpty (MyNode 'X' MyEmpty MyEmpty)))
--
--MyNode 'X' (MyNode 'X' (MyNode 'X' (MyNode 'X' MyEmpty MyEmpty) MyEmpty) (MyNode 'X' MyEmpty MyEmpty)) (MyNode 'X' (MyNode 'X' MyEmpty MyEmpty) (MyNode 'X' MyEmpty MyEmpty))

-- 60

-- Construct height-balanced binary trees with a given number of nodes. 

-- Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?
-- 
-- Clearly, MaxN = 2H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height H.
-- 
-- On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a function maxHeight that computes this.
-- 
-- Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. Find out how many height-balanced trees exist for N = 15.
-- 
-- Example in Prolog:
-- 
-- ?- count_hbal_trees(15,C).
-- C = 1553
-- 
-- Example in Haskell:
-- 
-- λ> length $ hbalTreeNodes 'x' 15
-- 1553
-- λ> map (hbalTreeNodes 'x') [0..3]
-- [[Empty],
--  [Branch 'x' Empty Empty],
--  [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
--  [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]

minNodes :: Int -> Int
minNodes 1 = 1
minNodes n = subMinNodes (n - 2)

subMinNodes :: Int -> Int
subMinNodes 0 = 1 + 1
subMinNodes n = 2^n + subMinNodes (n - 1)

maxHeight :: Int -> Int
maxHeight n = subMaxHeight n 1 1

subMaxHeight :: Int -> Int -> Int -> Int
subMaxHeight n n2 n3
    | n - n3 >= 0 = 
        let nodes = minNodes (n2 + 1)
        in subMaxHeight n (n2 + 1) nodes
    | otherwise = (n2 - 1)

-- ...

-- 61

-- Count the leaves of a binary tree. 
-- A leaf is a node with no successors. Write a predicate count_leaves/2 to count them. 
-- Example in Haskell:
-- 
-- λ> countLeaves tree4
-- 2

-- Collect the leaves of a binary tree in a list. 
-- A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list. 
-- Example in Haskell:
-- 
-- λ> leaves tree4
-- [4,2]

tree4 = MyNode 1 (MyNode 2 MyEmpty (MyNode 4 MyEmpty MyEmpty))
                 (MyNode 2 MyEmpty MyEmpty)

countLeaves :: (MyTree a) -> Int
countLeaves MyEmpty = 0
countLeaves (MyNode _ MyEmpty MyEmpty) = 1
countLeaves (MyNode _ l r) = countLeaves l + countLeaves r

-- 61A

grepLeaves :: (MyTree a) -> [a]
grepLeaves MyEmpty = []
grepLeaves (MyNode x MyEmpty MyEmpty) = [x]
grepLeaves (MyNode _ l r) = grepLeaves l ++ grepLeaves r


-- 62

-- Collect the internal nodes of a binary tree in a list. 
-- An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list. 

-- Example in Haskell:
-- 
-- λ> internals tree4
-- [1,2]

grepNonEmpty :: (MyTree a) -> [a]
grepNonEmpty MyEmpty = []
grepNonEmpty (MyNode _ MyEmpty MyEmpty) = []
grepNonEmpty (MyNode x l r) = [x] ++ grepNonEmpty l ++ grepNonEmpty r


-- 62B

-- Collect the nodes at a given level in a list. 
-- A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list. 

-- Example in Haskell:

-- λ> atLevel tree4 2
-- [2,2]

atLevel :: (MyTree a) -> Int -> [a]
atLevel tree lvl = subAtLevel tree lvl 1

subAtLevel :: (MyTree a) -> Int -> Int -> [a]
subAtLevel (MyNode x l r) n n2
    | n /= n2 = subAtLevel l n (n2 + 1) ++ subAtLevel r n (n2 + 1)
    | otherwise = [x]


-- 63

-- Construct a complete binary tree. 

-- A complete binary tree with height H is defined as follows:
-- 
--     The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
--     In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
-- 
-- Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
-- 
-- We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. For every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly construct a complete binary tree structure.
-- 
-- Write a predicate complete_binary_tree/2.
-- 
-- Example:
-- 
-- % complete_binary_tree(N,T) :- T is a complete binary tree with N nodes.
-- 
-- Example in Haskell:
-- 
-- λ> completeBinaryTree 4
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
-- 
-- λ> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
-- True

completeBinaryTree :: Int -> (MyTree Char)
completeBinaryTree n = subCompleteBinaryTree n

subCompleteBinaryTree :: Int -> (MyTree Char)
subCompleteBinaryTree 0 = MyEmpty
subCompleteBinaryTree 1 = MyNode 'X' MyEmpty MyEmpty
subCompleteBinaryTree n = 
    let nr = (n - 1) `div` 2
        nl = (n - 1) - nr 
        l = subCompleteBinaryTree nl
        r = subCompleteBinaryTree nr
    in MyNode 'X' l r

--MyNode 'X' (MyNode 'X' (MyNode 'X' (MyNode 'X' MyEmpty MyEmpty) MyEmpty) (MyNode 'X' MyEmpty MyEmpty)) (MyNode 'X' (MyNode 'X' MyEmpty MyEmpty) (MyNode 'X' MyEmpty MyEmpty))

isCompleteBinary :: (MyTree Char) -> Bool
isCompleteBinary tree = 
    let treedepth = findMaxDepthBinaryTreeHbal tree
        xs = subIsCompleteBinary tree 1 treedepth
    in subIsCompleteBinaryVerify (tail xs) 2 (head xs) (treedepth - 1)

subIsCompleteBinary :: (MyTree Char) -> Int -> Int -> [Int]
subIsCompleteBinary MyEmpty n _ = [-n + 1]
subIsCompleteBinary (MyNode _ MyEmpty MyEmpty) n treedepth = if n == treedepth
                                                             then [n - 1]
                                                             else  [-n, -n]
subIsCompleteBinary (MyNode _ l r) n treedepth = subIsCompleteBinary l (n + 1) treedepth ++ subIsCompleteBinary r (n + 1) treedepth

subIsCompleteBinaryVerify :: [Int] -> Int -> Int -> Int -> Bool
subIsCompleteBinaryVerify [] _ _ _ = True
subIsCompleteBinaryVerify (x:xs) n lst treedepth
    | (x > 0) && (n `mod` 2 == 0) && (lst < 0) = False
    | abs (treedepth - abs(x)) > 0 = False
    | otherwise = subIsCompleteBinaryVerify xs (n + 1) x treedepth

findMaxDepthBinaryTreeHbal :: (MyTree Char) -> Int
findMaxDepthBinaryTreeHbal tree = subFindMaxDepthBinaryTreeHbal tree 0

subFindMaxDepthBinaryTreeHbal :: (MyTree Char) -> Int -> Int
subFindMaxDepthBinaryTreeHbal MyEmpty n = n
subFindMaxDepthBinaryTreeHbal (MyNode _ l r) n = subFindMaxDepthBinaryTreeHbal l (n + 1)

-- 64

-- Layout algorithm for displaying trees. 

-- Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration below:

-- see tree1.jpg

-- In this layout strategy, the position of a node v is obtained by the following two rules:
-- 
--     x(v) is equal to the position of the node v in the inorder sequence
--     y(v) is equal to the depth of the node v in the tree
-- 
-- Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.
-- 
-- Here is the example tree from the above illustration:
-- 
-- tree64 = Branch 'n'
--                 (Branch 'k'
--                         (Branch 'c'
--                                 (Branch 'a' Empty Empty)
--                                 (Branch 'h'
--                                         (Branch 'g'
--                                                 (Branch 'e' Empty Empty)
--                                                 Empty
--                                         )
--                                         Empty
--                                 )
--                         )
--                         (Branch 'm' Empty Empty)
--                 )
--                 (Branch 'u'
--                         (Branch 'p'
--                                 Empty
--                                 (Branch 's'
--                                         (Branch 'q' Empty Empty)
--                                         Empty
--                                 )
--                         )
--                         Empty
--                 )
-- 
-- Example in Haskell:
-- 
-- λ> layout tree64
-- Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...

tree64 = MyNode 'n'
                (MyNode 'k'
                        (MyNode 'c'
                                (MyNode 'a' MyEmpty MyEmpty)
                                (MyNode 'h'
                                        (MyNode 'g'
                                                (MyNode 'e' MyEmpty MyEmpty)
                                                MyEmpty
                                        )
                                        MyEmpty
                                )
                        )
                        (MyNode 'm' MyEmpty MyEmpty)
                )
                (MyNode 'u'
                        (MyNode 'p'
                                MyEmpty
                                (MyNode 's'
                                        (MyNode 'q' MyEmpty MyEmpty)
                                        MyEmpty
                                )
                        )
                        MyEmpty
                )

layout :: (MyTree Char) -> [(Char, (Int, Int))]
layout tree = subLayout tree 0 0 [] 0

subLayout :: (MyTree Char) -> Int -> Int 
             -> [(Char, (Int, Int))] -> Int -> [(Char, (Int, Int))]
subLayout MyEmpty _ _ xs _ = xs
subLayout (MyNode x l r) depth pos xs lastright = 
    let newpos = subCountNodes l
        newdepth = depth + 1
        lval   = subLayout l newdepth newpos ((x, (newpos + lastright, newdepth)):xs) lastright
        rval   = subLayout r newdepth (pos + newpos) [] (lastright + newpos)
    in lval ++ rval

countNodes :: (MyTree Char) -> Int
countNodes tree = subCountNodes tree - 1

subCountNodes :: (MyTree Char) -> Int
subCountNodes MyEmpty = 1
subCountNodes (MyNode _ l r) = (subCountNodes l) + (subCountNodes r)

-- 65

-- Layout algorithm for displaying trees (part 2). 
-- An alternative layout method is depicted in the illustration below: 
-- see tree2.jpg

-- Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance between neighboring nodes is constant.
-- 
-- Use the same conventions as in problem P64 and test your function in an appropriate way.
-- 
-- Here is the example tree from the above illustration:
-- 
-- tree65 = Branch 'n'
--                 (Branch 'k'
--                         (Branch 'c'
--                                 (Branch 'a' Empty Empty)
--                                 (Branch 'e'
--                                         (Branch 'd' Empty Empty)
--                                         (Branch 'g' Empty Empty)
--                                 )
--                         )
--                         (Branch 'm' Empty Empty)
--                 )
--                 (Branch 'u'
--                         (Branch 'p'
--                                 Empty
--                                 (Branch 'q' Empty Empty)
--                         )
--                         Empty
--                 )
-- 
-- Example in Haskell:
-- 
-- λ> layout tree65
-- Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...

tree65 = MyNode 'n'
                (MyNode 'k'
                        (MyNode 'c'
                                (MyNode 'a' MyEmpty MyEmpty)
                                (MyNode 'e'
                                        (MyNode 'd' MyEmpty MyEmpty)
                                        (MyNode 'g' MyEmpty MyEmpty)
                                )
                        )
                        (MyNode 'm' MyEmpty MyEmpty)
                )
                (MyNode 'u'
                        (MyNode 'p'
                                MyEmpty
                                (MyNode 'q' MyEmpty MyEmpty)
                        )
                        MyEmpty
                )

findMaxDepthBinaryTree :: (MyTree Char) -> Int
findMaxDepthBinaryTree tree = myMax $ subFindMaxDepthBinaryTree tree 0

subFindMaxDepthBinaryTree :: (MyTree Char) -> Int -> [Int]
subFindMaxDepthBinaryTree MyEmpty n = [n]
subFindMaxDepthBinaryTree (MyNode _ l r) n = subFindMaxDepthBinaryTree l (n  + 1) ++ subFindMaxDepthBinaryTree r (n + 1)

findMaxDepthBinaryTree2 :: (MyTree Char) -> Int
findMaxDepthBinaryTree2 tree = subFindMaxDepthBinaryTree2 tree 0

subFindMaxDepthBinaryTree2 :: (MyTree Char) -> Int -> Int
subFindMaxDepthBinaryTree2 MyEmpty n = n
subFindMaxDepthBinaryTree2 (MyNode _ l r) n = 
    let val1 = subFindMaxDepthBinaryTree2 l (n + 1)
        val2 = subFindMaxDepthBinaryTree2 r (n + 1)
    in max val1 val2

layout2 :: (MyTree Char) -> [(Char, (Int, Int))]
layout2 tree = 
    let maxdepth = findMaxDepthBinaryTree2 tree
    in subLayout2 tree maxdepth 0 0 False False
        
subLayout2 :: (MyTree Char) -> Int -> Int -> 
                Int -> Bool -> Bool -> [(Char, (Int, Int))]
subLayout2 MyEmpty _ _ _ _ _ = []
subLayout2 (MyNode x l r) maxdepth depth lastright fromright alrd =
    let newdepth = depth + 1
        topval   = (2 ^ (maxdepth - newdepth) `div` 2)
        val = if not fromright
              then if not alrd
                   then (spaceCalc l (maxdepth - newdepth) 1) + topval
                   else lastright + (spaceCalc l (maxdepth - newdepth) 1)
              else lastright + (2 ^ (maxdepth - newdepth))
        newlastright = if not fromright
                       then lastright
                       else val - topval
    in [(x, (val, newdepth))] ++ (subLayout2 l maxdepth newdepth newlastright False alrd) ++ (subLayout2 r maxdepth newdepth val True True)

spaceCalc :: (MyTree Char) -> Int -> Int -> Int
spaceCalc MyEmpty _ _ = 0
spaceCalc (MyNode _ MyEmpty _) _ n2 = n2
spaceCalc (MyNode x l r) n n2 = (spaceCalc l (n - 1) (n2 + 2^n `div` 4))

-- 66

-- Layout algorithm for displaying trees (part 3). 
-- Yet another layout strategy is shown in the illustration below: 
-- see tree3.jpg

-- The method yields a very compact layout while maintaining a certain symmetry in every node. Find out the rules and write the corresponding Prolog predicate. Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree?
-- 
-- Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way. Note: This is a difficult problem. Don't give up too early!
-- 
-- Which layout do you like most?
-- 
-- Example in Haskell:
-- 
-- λ> layout tree65
-- Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...

layout3 :: (MyTree Char) -> [(Char, (Int, Int))]
layout3 tree = subLayout3a tree [] 0 0 False False 0

subLayout3a :: (MyTree Char) -> [(Char, (Int, Int))] -> 
                Int -> Int -> Bool -> Bool -> Int -> [(Char, (Int, Int))]
subLayout3a MyEmpty xs _ _ _ _ _ = xs
subLayout3a (MyNode x l r) xs depth lastright fromright alrd lastval = 
    let val = if not alrd
              then subLayout3Preb (MyNode x l r) + 1
              else if fromright
                   then lastright + lastval
                   else lastright - lastval
        newdepth = depth + 1
        newlastval = subLayout3b (MyNode x l r)
    in subLayout3a l ((x, (val, newdepth)):xs) newdepth val False alrd newlastval ++ subLayout3a r [] newdepth val True True newlastval

subLayout3Preb :: (MyTree Char) -> Int
subLayout3Preb tree = (subLayout3Preb2 tree)

subLayout3Preb2 :: (MyTree Char) -> Int
subLayout3Preb2 MyEmpty = 0
subLayout3Preb2 (MyNode _ MyEmpty _) = 0
subLayout3Preb2 (MyNode x l r) = subLayout3b (MyNode x l r) + subLayout3Preb2 l

subLayout3b :: (MyTree Char) -> Int
subLayout3b (MyNode _ _ MyEmpty) = 1
subLayout3b (MyNode _ MyEmpty _) = 1
subLayout3b (MyNode _ l r) = (min (subLayout3bLeft l 1) (subLayout3bRight r 1))

subLayout3bRight :: (MyTree Char) -> Int -> Int
subLayout3bRight (MyNode _ MyEmpty _) n = n
subLayout3bRight (MyNode _ l _) n = subLayout3bRight l (n + 1)

subLayout3bLeft :: (MyTree Char) -> Int -> Int
subLayout3bLeft (MyNode _ _ MyEmpty) n = n
subLayout3bLeft (MyNode _ _ r) n = subLayout3bLeft r (n + 1)

findLastVal :: (MyTree Char) -> (MyTree Char) -> Int
findLastVal MyEmpty MyEmpty = 0
findLastVal _ MyEmpty       = 1
findLastVal MyEmpty _       = 1
findLastVal _ _ = 2

-- 67A

-- A string representation of binary trees. 

-- Somebody represents binary trees as strings of the following type:
-- 
--     a(b(d,e),c(,f(g,)))
-- 
-- a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.
-- 
-- Example in Prolog
-- 
-- ?- tree_to_string(t(x,t(y,nil,nil),t(a,nil,t(b,nil,nil))),S).
-- S = 'x(y,a(,b))'
-- ?- string_to_tree('x(y,a(,b))',T).
-- T = t(x, t(y, nil, nil), t(a, nil, t(b, nil, nil)))
-- 
-- Example in Haskell:
-- 
-- λ> stringToTree "x(y,a(,b))" >>= print
-- Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
-- λ> let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)
-- True

parTokenizer :: [Char] -> ([Int], [Int])
parTokenizer xs = subParTokenizer xs 0 0 [] []

subParTokenizer :: [Char] -> Int -> Int -> [Int] -> [Int] -> ([Int], [Int])
subParTokenizer [] _ _ ids parxs = (ids, parxs)
subParTokenizer (x:xs) idx parnb ids parxs
    | x == '('  = subParTokenizer xs (idx + 1) (parnb + 1) (ids ++ [idx]) (parxs ++ [parnb])
    | x == ')'  = subParTokenizer xs (idx + 1) (parnb - 1) (ids ++ [idx]) (parxs ++ [parnb - 1])
    | otherwise = subParTokenizer xs (idx + 1) parnb ids parxs

testString :: [Char]
testString = "a(b(d,e),c(i(i,i),f(g,k)))"

testString1 :: [Char]
testString1 = "a(b(d,e),c(,f(g,k)))"

testString1b :: [Char]
testString1b = "a(b(d,e),c(,f(,)))"

testString2 :: [Char]
testString2 = "a(b(d,e),c(p,g))"

testString3 :: [Char]
testString3 = "a(b,c)"

stringToTree :: [Char] -> (MyTree Char)
stringToTree [] = MyEmpty
stringToTree [x] = MyNode x MyEmpty MyEmpty
stringToTree xs =
    let val = head xs
        interxs = init . tail . tail $ xs 
        (ids, parxs) = parTokenizer interxs
        zeroids = reverse $ grepn2 0 parxs
        newids = getRangeList ids zeroids
        newparxs = getRangeList parxs zeroids
        idx = if length newids > 2
              then ids !! (stopAt 0 3 newparxs)
              else if length newids == 0
                   then if head interxs == ','
                        then 1
                        else 3
                   else if head interxs == ','
                   then 1
                   else length (interxs) + 1
        (llist, rlist) = splitAt (idx - 1) interxs
        llist2 = if length llist > 0
                 then init llist
                 else []
        rlist2 = if length rlist > 0
                 then if head rlist == ','
                      then tail rlist
                      else rlist
                 else rlist
    in MyNode val (stringToTree llist2) (stringToTree rlist2)

treeToString :: (MyTree Char) -> [Char]
treeToString (MyNode x MyEmpty MyEmpty) = [x]
treeToString MyEmpty = []
treeToString (MyNode x l r) = [x] ++ ['('] ++ treeToString l ++ [','] ++ treeToString r ++ [')']

-- 70C

-- Multiway Trees
-- 
-- A multiway tree is composed of a root element and a (possibly empty) set of successors which are multiway trees themselves. A multiway tree is never empty. The set of successor trees is sometimes called a forest. 
-- see tree4.jpg

-- Count the nodes of a multiway tree. 

-- Example in Haskell:
-- 
-- λ> nnodes tree2
-- 2

data MTree a = MNode a [MTree a] deriving (Show, Eq)

mtree1 = MNode 'a' []

mtree2 = MNode 'a' [MNode 'b' []]

mtree3 = MNode 'a' [MNode 'b' [MNode 'c' []]]

mtree4 = MNode 'b' [MNode 'd' [], MNode 'e' []]

mtree5 = MNode 'a' [
                MNode 'f' [MNode 'g' []],
                MNode 'c' [],
                MNode 'b' [MNode 'd' [], MNode 'e' []]
                ]

mnodes :: (MTree a) -> Int
mnodes mtree = subMNodes mtree + 1

subMNodes :: (MTree a) -> Int
subMNodes (MNode _ next) = foldl (\acc x -> acc + 1 + subMNodes x) 0 next


-- 70, revisited lol

-- Construct a multiway tree from a node string. 

-- We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack to the previous level.
-- 
-- By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^ 

-- see tree5.jpg

-- Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given. Make your predicate work in both directions.
-- 
-- Example in Haskell:
-- 
-- λ> stringToTree "afg^^c^bd^e^^^"
-- Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
-- 
-- λ> treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])
-- "afg^^c^bd^e^^^"

stringmtree = "afg^^c^bd^e^^"

stringToMTree :: [Char] -> (MTree Char)
stringToMTree (x:xs) = subStringToMTree xs [MNode x []]

subStringToMTree :: [Char] -> [(MTree Char)] -> (MTree Char)
subStringToMTree [] [mtree] = mtree
subStringToMTree (x:xs) trees
    | x /= '^'  = subStringToMTree xs ((MNode x []):trees)
    | otherwise = 
        let lastnode = head trees
            (MNode x restxs) = head . tail $ trees
            newnode = MNode x (restxs ++ [lastnode])
        in subStringToMTree xs (newnode:(tail . tail $ trees))


-- 71

-- Determine internal path length of multiway tree. 

-- We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, tree5 has an internal path length of 9.
-- 
-- Example in Haskell:
-- 
-- λ> ipl tree5
-- 9
-- λ> ipl tree4
-- 2

ipl :: (MTree a) -> Int
ipl mtree = subIpl mtree 1

subIpl :: (MTree a) -> Int -> Int
subIpl (MNode _ next) n = foldl (\acc x -> acc + n + subIpl x (n + 1)) 0 next

-- 72

-- Construct bottom-up order sequence of multiway tree nodes. 
-- Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the multiway tree Tree.
-- 
-- Example in Haskell:
-- 
-- λ> bottom_up tree5
-- "gfcdeba"


bottom_up :: (MTree Char) -> [Char]
bottom_up (MNode x restxs) = concat (map bottom_up restxs) ++ [x]

-- 73
-- Lisp-like multiway tree representation. 
-- There is a particular notation for multiway trees in Lisp. Lisp is a prominent functional programming language, which is used primarily for artificial intelligence problems. As such it is one of the main competitors of Prolog. In Lisp almost everything is a list, just as in Prolog everything is a term.
-- 
-- The following pictures show how multiway tree structures are represented in Lisp. 

-- see tree6.png
-- Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')', which we shall collectively call "tokens". We can represent this sequence of tokens as a Prolog list; e.g. the lispy expression (a (b c)) could be represented as the Prolog list ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the "lispy token list" LTL if the tree is given as term T in the usual Prolog notation.
-- 
-- (The Prolog example given is incorrect.)
-- 
-- Example in Haskell:
-- 
-- λ> display lisp tree1
-- "a"
-- λ> display lisp tree2
-- "(a b)"
-- λ> display lisp tree3
-- "(a (b c))"
-- λ> display lisp tree4
-- "(b d e)"
-- λ> display lisp tree5
-- "(a (f g) c (b d e))"
-- 
-- As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that the inverse conversion is also possible. 

display :: (MTree Char) -> [Char]
display (MNode x []) = [' ', x]
display (MNode x restxs) = ['('] ++ [x] ++ [' '] ++ concat (map display restxs) ++ [')']


-- 80, Wooooow that was wayyy more straightforward than the current solution on haskell.org lol

-- Graphs
-- 
-- A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes. 
-- see graph1.jpg

-- There are several ways to represent graphs in Prolog. One method is to represent each edge separately as one clause (fact). In this form, the graph depicted below is represented as the following predicate:
-- 
-- edge(h,g).
-- edge(k,f).
-- edge(f,b).
-- ...
-- 
-- We call this edge-clause form. Obviously, isolated nodes cannot be represented. Another method is to represent the whole graph as one data object. According to the definition of the graph as a pair of two sets (nodes and edges), we may use the following Prolog term to represent the example graph:
-- 
-- graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])
-- 
-- We call this graph-term form. Note, that the lists are kept sorted, they are really sets, without duplicated elements. Each edge appears only once in the edge list; i.e. an edge from a node x to another node y is represented as e(x,y), the term e(y,x) is not present. The graph-term form is our default representation. In SWI-Prolog there are predefined predicates to work with sets.
-- 
-- A third representation method is to associate with each node the set of nodes that are adjacent to that node. We call this the adjacency-list form. In our example:
-- 
-- [n(b,[c,f]), n(c,[b,f]), n(d,[]), n(f,[b,c,k]), ...]
-- 
-- The representations we introduced so far are Prolog terms and therefore well suited for automated processing, but their syntax is not very user-friendly. Typing the terms by hand is cumbersome and error-prone. We can define a more compact and "human-friendly" notation as follows: A graph is represented by a list of atoms and terms of the type X-Y (i.e. functor '-' and arity 2). The atoms stand for isolated nodes, the X-Y terms describe edges. If an X appears as an endpoint of an edge, it is automatically defined as a node. Our example could be written as:
-- 
-- [b-c, f-c, g-h, d, f-b, k-f, h-g]
-- 
-- We call this the human-friendly form. As the example shows, the list does not have to be sorted and may even contain the same edge multiple times. Notice the isolated node d. (Actually, isolated nodes do not even have to be atoms in the Prolog sense, they can be compound terms, as in d(3.75,blue) instead of d in the example). 

-- see graph2.jpg

-- When the edges are directed we call them arcs. These are represented by ordered pairs. Such a graph is called directed graph. To represent a directed graph, the forms discussed above are slightly modified. The example graph above is represented as follows:
-- 
-- Arc-clause form
-- 
-- arc(s,u).
-- arc(u,r).
-- ...
-- 
-- Graph-term form
-- 
-- digraph([r,s,t,u,v],[a(s,r),a(s,u),a(u,r),a(u,s),a(v,u)])
-- 
-- Adjacency-list form
-- 
-- [n(r,[]),n(s,[r,u]),n(t,[]),n(u,[r]),n(v,[u])]
-- 
-- Note that the adjacency-list does not have the information on whether it is a graph or a digraph.
-- 
-- Human-friendly form
-- 
-- [s > r, t, u > r, s > u, u > s, v > u] 
-- 
-- Finally, graphs and digraphs may have additional information attached to nodes and edges (arcs). For the nodes, this is no problem, as we can easily replace the single character identifiers with arbitrary compound terms, such as city('London',4711). On the other hand, for edges we have to extend our notation. Graphs with additional information attached to edges are called labelled graphs. 

-- see graph3.jpg

-- Arc-clause form
-- 
-- arc(m,q,7).
-- arc(p,q,9).
-- arc(p,m,5).
-- 
-- Graph-term form
-- 
-- digraph([k,m,p,q],[a(m,p,7),a(p,m,5),a(p,q,9)])
-- 
-- Adjacency-list form
-- 
-- [n(k,[]),n(m,[q/7]),n(p,[m/5,q/9]),n(q,[])]
-- 
-- Notice how the edge information has been packed into a term with functor '/' and arity 2, together with the corresponding node.
-- 
-- Human-friendly form
-- 
-- [p>q/9, m>q/7, k, p>m/5]
-- 
-- The notation for labelled graphs can also be used for so-called multi-graphs, where more than one edge (or arc) are allowed between two given nodes. 

-- Conversions. 

-- Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because it's a lot of work to deal with all the special cases.
-- 
-- Example in Haskell:
-- 
-- λ> graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
-- Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]

type Graph = ([Char], [(Char, Char)])

mygraph :: Graph
mygraph = (['b','c','d','f','g','h','k'], [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')])

type Adj = [(Char, [Char])]

graphToAdj :: Graph -> Adj
graphToAdj ([], xs2) = []
graphToAdj ((x:xs), xs2) =
    let fstval = map snd (filter (\(val1, _) -> val1 == x) xs2)
        sndval = map fst (filter (\(_, val2) -> val2 == x) xs2)
    in [(x, fstval ++ sndval)] ++ graphToAdj (xs, xs2)


-- 81

-- Paths between two given nodes. 
-- Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.

-- Example in Haskell:
-- 
-- λ> paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- [[1,2,3,4],[1,3,4]]
-- λ> paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- []

myGraph :: [(Int, Int)]
myGraph = [(1,2), (2,3), (1,3), (3,4), (4,2), (5,6)]

path :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
path n1 n2 xs = subPath n1 n2 xs []

subPath :: (Eq a) => a -> a -> [(a, a)] -> [a] -> [[a]]
subPath n1 n2 xs outxs
    | n1 /= n2 = 
        let newxs = filter (\(val1, _) -> val1 == n1) xs
        in if not . null $ newxs
           then concat $ map (\(_, newn1) -> subPath newn1 n2 xs (outxs ++ [n1])) newxs
           else []
    | otherwise = [outxs ++ [n2]]
        

-- 82

-- Cycle from a given node. 

-- Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. The predicate should return all cycles via backtracking.
-- 
-- Example in Haskell:
-- 
-- λ> cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- [[2,3,4,2]]
-- λ> cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- []

myGraph2 :: [(Int, Int)]
myGraph2 = [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

myGraph2b :: ([Int], [(Int, Int)])
myGraph2b = ([1..6], [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])

myCycle :: (Eq a) => a -> [(a, a)] -> [a]
myCycle x xs = 
    let outxs = subCycle x x xs [] False 0 (length xs)
    in if null outxs
       then []
       else outxs

subCycle :: (Eq a) => a -> a -> [(a, a)] -> [a] -> Bool -> Int -> Int -> [a]
subCycle _ _ [] _ _ _ _ = []
subCycle x cmp xs outxs alrd n cmp2
    | x /= cmp  = 
        let newxs = filter (\(val1, _) -> val1 == x) xs
        in if (not . null $ newxs) && n < cmp2
           then concat $ map (\(_, n2) -> subCycle n2 cmp xs (outxs ++ [x]) True (n + 1) cmp2) newxs
           else []
    | otherwise = if alrd
                  then outxs ++ [cmp]
                  else let newxs = filter (\(val1, _) -> val1 == x) xs
                       in if not . null $ newxs
                          then concat $ map (\(_, n2) -> subCycle n2 cmp xs (outxs ++ [x]) True (n + 1) cmp2) newxs
                          else []


-- 83

-- Construct all spanning trees. 
-- Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph. With this predicate, find out how many spanning trees there are for the graph depicted to the left. The data of this example graph can be found in the file p83.dat. When you have a correct solution for the s_tree/2 predicate, use it to define two other useful predicates: is_tree(Graph) and is_connected(Graph). Both are five-minutes tasks! 
-- see graph4.jpg

-- Example in Haskell:
-- 
-- λ> length $ spanningTree k4
-- 16

graph83 = Graph2 ['a','b','c','d','e','f','g','h'] [ ('a','b'), ('a','d')
    , ('b','c'), ('b','e')
    , ('c','e')
    , ('d','e'), ('d','f'), ('d','g')
    , ('e','h')
    , ('f','g')
    , ('g','h')
    ]

graph83b = (['a','b','c','d','e','f','g','h'], [ ('a','b'), ('a','d')
    , ('b','c'), ('b','e')
    , ('c','e')
    , ('d','e'), ('d','f'), ('d','g')
    , ('e','h')
    , ('f','g')
    , ('g','h')
    ])

data Graph2 a = Graph2 [a] [(a, a)] deriving (Show, Eq)

k4 = Graph2 ['a', 'b', 'c', 'd'] [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

k4b = (['a', 'b', 'c', 'd'], [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')])

--paths' :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
--paths' a b xs | a == b = [[a]]
--              | otherwise = concat [map (a :) $ paths' d b $ [x | x <- xs, x /= (c, d)]
--                                   | (c, d) <- xs, c == a] ++ 
--                            concat [map (a :) $ paths' c b $ [x | x <- xs, x /= (c, d)]
--                                   | (c, d) <- xs, d == a]
--
--cycle' :: (Eq a) => a -> [(a, a)] -> [[a]]
--cycle' a xs = [a : path | e <- xs, fst e == a, path <- paths' (snd e) a [x | x <- xs, x /= e]] ++
--              [a : path | e <- xs, snd e == a, path <- paths' (fst e) a [x | x <- xs, x /= e]]
--
--spantree :: (Eq a) => Graph2 a -> [Graph2 a]
--spantree (Graph2 xs ys) = filter (not . cycles) $ filter (nodes) alltrees
--   where
--      alltrees = [Graph2 (ns edges) edges | edges <- foldr acc [[]] ys]
--      acc e es = es ++ (map (e:) es)
--      ns e = foldr (\x xs -> if x `elem` xs then xs else x:xs) 
--             [] $ concat $ map (\(a, b) -> [a, b]) e
--      nodes (Graph2 xs' ys') = length xs - 1 == length ys' && length xs' == length xs
--      cycles (Graph2 xs' ys') = any ((/=) 0 . length . flip cycle' ys') xs'

spantree2 :: ([Char], [(Char, Char)]) -> [([Char], [(Char, Char)])]
spantree2 (xs, ys) = filter isConnected $ filter (noncycle) alltrees
   where
      alltrees = [((uniqueval edges), edges) | edges <- foldr acc [[]] ys]
      acc e es = es ++ (map (e:) es)
      uniqueval e = foldr (\x xs -> if x `elem` xs then xs else x:xs) 
             [] (concat $ map (\(a, b) -> [a, b]) e)
      noncycle (xs', ys') = length xs - 1 == length ys' -- && (length $ unique (xs')) == length xs

--isCycle :: (Eq a) => ([a], [(a, a)]) -> Bool
--isCycle ([], xs2) = False
--isCycle ((x:xs), xs2) = 
--    let outxs = myCycle x xs2
--    in if null outxs
--       then isCycle (xs, xs2)
--       else True

testval = ("cbd", [('b','c'),('c','d'),('b','d')])


isConnected :: (Eq a) => ([a], [(a, a)]) -> Bool
isConnected (nodexs, edgexs) = 
    let newedgexs = edgexs ++ (map (\(x, y) -> (y, x)) edgexs)
        outxs = subIsConnected (nodexs, newedgexs) (length nodexs)
    in  outxs

subIsConnected :: (Eq a) => ([a], [(a, a)]) -> Int -> Bool
subIsConnected ((fstval:nodexs), edgexs) cmp = 
    let outxs = subIsConnected2 edgexs [fstval] fstval cmp 
    in cmp == (length . unique $ outxs)

subIsConnected2 :: (Eq a) => [(a, a)] -> [a] -> a -> Int -> [a]
subIsConnected2 xs outxs n cmp
    | length outxs == cmp = outxs
    | otherwise = 
        let newxs = filter (\(x, _) -> x == n) xs
        in  if null newxs
            then outxs
            else concat $ map (\(_, x2) -> subIsConnected2 xs (x2:outxs) x2 cmp) newxs

--subIsConnected :: (Eq a) => ([a], [(a, a)]) -> Int -> [Bool]
--subIsConnected ([], _) _ = []
--subIsConnected ((fstval:nodexs), edgexs) cmp = 
--    let outxs = subIsConnected2 edgexs [fstval] fstval cmp 
--    in [cmp == (length . unique $ outxs)] ++ subIsConnected (nodexs, edgexs) cmp


-- Bonus question, find the directions of a graph

findDirection :: (Eq a) => ([a], [(a, a)]) -> [[[a]]]
findDirection (nodexs, edgexs) = 
    let outxs = subFindDirection (nodexs, edgexs) (length nodexs)
        newoutxs = filter (\(x1, _) -> x1) outxs
    in  map snd newoutxs

subFindDirection :: (Eq a) => ([a], [(a, a)]) -> Int -> [(Bool, [[a]])]
subFindDirection ([], _) _ = []
subFindDirection ((fstval:nodexs), edgexs) cmp = 
    let outxs = subFindDirection2 edgexs [fstval] fstval cmp 
    in [(cmp == (length . unique . concat $ outxs), outxs)] ++ (subFindDirection (nodexs, edgexs) cmp)

subFindDirection2 :: (Eq a) => [(a, a)] -> [a] -> a -> Int -> [[a]]
subFindDirection2 xs outxs n cmp
    | length outxs == cmp = [outxs]
    | otherwise = 
        let newxs = filter (\(x, _) -> x == n) xs
        in  if null newxs
            then [outxs]
            else concat $ map (\(_, x2) -> subFindDirection2 xs (x2:outxs) x2 cmp) newxs


-- 84

-- Construct the minimal spanning trees.

-- Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal spanning tree of a given labelled graph. Hint: Use the algorithm of Prim. A small modification of the solution of P83 does the trick. The data of the example graph to the right can be found in the file p84.dat. 

-- see graph5.jpg

-- Example in Haskell:
-- 
-- λ> prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
-- [(1,2,12),(1,3,34),(2,4,55),(2,5,32)]

graph84 = (['1','2','3','4','5'], [('1','2',12),('1','3',34),('1','5',78),('2','4',55),('2','5',32),('3','4',61),('3','5',44),('4','5',93)]) :: ([Char], [(Char, Char, Int)])

minimalSpantree :: ([Char], [(Char, Char, Int)]) -> (([Char], [(Char, Char, Int)]), Int)
minimalSpantree (xs, ys) = 
    let preoutxs = map (\(nodesmp, xsmp) -> ((nodesmp, xsmp), (foldl (\acc (_, _, mpval) -> acc + mpval) 0 xsmp))) (filter isConnectedCalc $ filter (noncycle) alltrees)
        vals = map (\(_, x) -> x) preoutxs
        minval = myMin vals
    in  head $ filter (\(_, x) -> x == minval) preoutxs
   where
      alltrees = [((uniqueval edges), edges) | edges <- foldr acc [[]] ys]
      acc e es = es ++ (map (e:) es)
      uniqueval e = foldr (\x xs -> if x `elem` xs then xs else x:xs) 
             [] (concat $ map (\(a, b, _) -> [a, b]) e)
      noncycle (xs', ys') = length xs - 1 == length ys' && (length $ unique (xs')) == length xs

isConnectedCalc :: (Eq a) => ([a], [(a, a, Int)]) -> Bool
isConnectedCalc (nodexs, edgexs) = 
    let newedgexs = edgexs ++ (map (\(x, y, z) -> (y, x, z)) edgexs)
        outxs = subIsConnectedCalc (nodexs, newedgexs) (length nodexs)
    in  outxs

subIsConnectedCalc :: (Eq a) => ([a], [(a, a, Int)]) -> Int -> Bool
subIsConnectedCalc ((fstval:nodexs), edgexs) cmp = 
    let outxs = subIsConnectedCalc2 edgexs [fstval] fstval cmp 
    in cmp == (length . unique $ outxs)

subIsConnectedCalc2 :: (Eq a) => [(a, a, Int)] -> [a] -> a -> Int -> [a]
subIsConnectedCalc2 xs outxs n cmp
    | length outxs == cmp = outxs
    | otherwise = 
        let newxs = filter (\(x, _, _) -> x == n) xs
        in  if null newxs
            then outxs
            else concat $ map (\(_, x2, _) -> subIsConnectedCalc2 xs (x2:outxs) x2 cmp) newxs


-- 85

-- Graph isomorphism. 

-- Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f: N1 -> N2 such that for any nodes X,Y of N1, X and Y are adjacent if and only if f(X) and f(Y) are adjacent.
-- 
-- Write a predicate that determines whether two graphs are isomorphic. Hint: Use an open-ended list to represent the function f.
-- 
-- Example in Haskell:
-- 
-- λ> graphG1 = [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
-- λ> graphH1 = [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
-- λ> iso graphG1 graphH1
-- True

graphG1 = ([1,2,3,4,5,6,7,8], [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]) :: ([Int], [(Int, Int)])

graphH1 = ([1,2,3,4,5,6,7,8], [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]) :: ([Int], [(Int, Int)])

ng1 = [1,2,3,4,5,6,7,8] :: [Int] 
ng2 = [1,2,3,4,5,6,7,8] :: [Int]
dgs = [2,3,3,2,3,2,3,3] :: [Int]
dgs2 = [3,3,2,3,2,2,3,3] :: [Int]
dgs3 = [3,3,3,3,3,3,3,3] :: [Int]

iso :: (Eq a) => ([a], [(a, a)]) -> ([a], [(a, a)]) -> ([(a, a)], Bool)
iso (n1, ed1) (n2, ed2)
    | length n1 /= length n2   = ([], False)
    | length ed1 /= length ed2 = ([], False)
    | otherwise = 
        let degreexs1 = quickSortAsc $ findDegree (n1, ed1)
            degreexs2 = quickSortAsc $ findDegree (n2, ed2)
        in if degreexs1 /= degreexs2
           then ([], False)
           else
               let outxs = findGraphPermutation (n1, ed1) (n2, ed2) degreexs1 degreexs2 ed1 ed2
               in (outxs, (length outxs) /= 0)

findGraphPermutation :: (Eq a) => ([a], [(a, a)]) -> 
                        ([a], [(a, a)]) -> [Int] -> [Int] -> [(a, a)] -> [(a, a)] -> [(a, a)]
findGraphPermutation (n1, ed1) (n2, ed2) ids1 ids2 cmped1 cmped2 = 
    let xs = groupByDegree ids1 n1 ids2 n2 (unique ids1)
    in subFindGraphPermutation xs cmped1 cmped2

groupByDegree :: (Eq a) => [Int] -> [a] -> [Int] -> [a] -> [Int] -> [([a], [a])]
groupByDegree _ _ _ _ [] = []
groupByDegree ids1 n1 ids2 n2 (cmp:nxs) = 
    let g1 = map (\(_, curnode) -> curnode) (filter (\(val, _) -> val == cmp) (zip ids1 n1))
        g2 = map (\(_, curnode) -> curnode) (filter (\(val, _) -> val == cmp) (zip ids2 n2))
    in [(g1, g2)] ++ groupByDegree ids1 n1 ids2 n2 nxs

subFindGraphPermutation :: (Eq a) => [([a], [a])] -> [(a, a)] -> [(a, a)] -> [(a, a)]
subFindGraphPermutation [] _ _ = []
subFindGraphPermutation ((n1, n2):xs) cmped1 cmped2 =
    let permu = breakAt (map (\[x, y] -> (x, y)) (sequence [n1, n2]))
        permu2 = genBijections permu
        outxs = case (find (\vala -> testMapping vala cmped1 cmped2) permu2) of 
                  Just x -> x
                  Nothing -> []
    in outxs ++ subFindGraphPermutation xs cmped1 cmped2

findDegree :: (Eq a) => ([a], [(a, a)]) -> [Int]
findDegree ([], _) = []
findDegree ((x:xs), nodexs) = [length $ filter (\(x1, y1) -> x1 == x || y1 == x) nodexs] ++ findDegree (xs, nodexs)

testMapping :: (Eq a) => [(a,a)] -> [(a,a)] -> [(a,a)] -> Bool
testMapping mapping ed1 ed2 =
  all (\e -> applyMapping mapping e `elem` ed2) ed1

applyMapping :: (Eq a) => [(a,a)] -> (a,a) -> (a,a)
applyMapping f (u,v) = (lookup2 u f, lookup2 v f)
  where
    lookup2 x mapping = case lookup x mapping of
                          Just y -> y
                          Nothing -> x

genBijections :: Eq b => [[(a,b)]] -> [[(a,b)]]
genBijections [] = [[]]
genBijections (grp:grps) = 
    concat (map (\(src, tgt) -> let filteredgrps = map (filter (\(_, y) -> y /= tgt)) grps
                                in map ((src, tgt):) (genBijections filteredgrps)) 
                grp)


-- 86

-- Node degree and graph coloration. 

-- a) Write a predicate degree(Graph,Node,Deg) that determines the degree of a given node.

-- b) Write a predicate that generates a list of all nodes of a graph sorted according to decreasing degree.
-- 
-- c) Use Welch-Powell's algorithm to paint the nodes of a graph in such a way that adjacent nodes have different colors.
-- 
-- Example in Haskell:
-- 
-- λ> kColor ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
-- [('a',1),('b',2),('c',1),('d',2),('e',3),('f',2),('g',1),('h',3),('i',3),('j',2)]
-- 
-- kgraph = (['a','b','c','d','e','f','g','h','i','j'], [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')])

-- a

degree :: (Eq a) => [(a, a)] -> a -> Int
degree xs cmp = foldl (\acc (x, y) -> if x == cmp || y == cmp then acc + 1 else acc) 0 xs

-- b

genSortedNodeDegree :: (Eq a) => ([a], [(a, a)]) -> [a]
genSortedNodeDegree (nxs, edgexs) = 
    let predeg = subGenSortedNodeDegree (nxs, edgexs)
    in  countDegAsc (zip nxs predeg) (quickSortDesc . unique $ predeg)

subGenSortedNodeDegree :: (Eq a) => ([a], [(a, a)]) -> [Int]
subGenSortedNodeDegree ([], _) = []
subGenSortedNodeDegree ((cmp:xs), edgexs) = [degree edgexs cmp] ++ subGenSortedNodeDegree (xs, edgexs)

countDegAsc :: [(a, Int)] -> [Int] -> [a]
countDegAsc xs [] = []
countDegAsc xs (cmp:unics) = (map (\(x2, _) -> x2) (filter (\(_, x) -> x == cmp) xs)) ++ countDegAsc xs unics


--c
-- coloring the more conneted nodes, lead to a sort of "compartimentalization" that allow to reuse same colors for each created compartiment later, it is why we decreasingly sort nodes according to their number of connection first

kcolor :: (Eq a) => ([a], [(a, a)]) -> [(a, Int)]
kcolor (nxs, edgexs) = 
    let outxs = genSortedNodeDegree (nxs, edgexs)
    in subKcolor edgexs 1 (zip outxs (replicate (length outxs) 0))

subKcolor :: (Eq a) => [(a, a)] -> Int -> [(a, Int)] -> [(a, Int)]
subKcolor _ _ [] = []
subKcolor _ n [(curnode, _)] = [(curnode, n)]
subKcolor edgexs n ((curnode, _):outxs) = 
    let newoutxs = subKcolor2 n outxs edgexs [curnode]
    in  [(curnode, n)]
        ++ (filter (\(_, val) -> val /= 0) newoutxs) 
        ++ subKcolor edgexs (n + 1) (filter (\(_, nb) -> nb == 0) newoutxs)

subKcolor2 :: (Eq a) => Int -> [(a, Int)] -> [(a, a)] -> [a] -> [(a, Int)]
subKcolor2 _ [] _ _ = []
subKcolor2 n ((cmp, _):xs) edgexs alrd = 
    let (newn, newalrd) = if null (filter (\(v1, v2) -> v1 == cmp && v2 `elem` alrd || v1 `elem` alrd && v2 == cmp) edgexs)
               then (n, (cmp:alrd))
               else (0, alrd)
    in [(cmp, newn)] ++ subKcolor2 n xs edgexs newalrd

-- 87

-- Depth-first order graph traversal (alternative solution). 

-- Write a predicate that generates a depth-first order graph traversal sequence. The starting point should be specified, and the output should be a list of nodes that are reachable from this starting point (in depth-first order).
-- 
-- Example in Haskell:
-- 
-- λ> depthFirst ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]) 1
-- [1,2,3,4,5]

graph87a = ([1,2,3,4,5,6,7], [(1,2),(2,4),(2,3), (1, 11), (11, 33)])

graph87b = ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7),(1,11),(1,5),(5,77)])

depthFirst :: (Eq a) => ([a], [(a, a)]) -> a -> [a]
depthFirst (_, xs) cmp = 
    let newxs = xs ++ (map (\(x, y) -> (y, x)) xs)
    in subDepthFirst (zip newxs (replicate (length newxs) False)) cmp [] [cmp] cmp

subDepthFirst :: (Eq a) => [((a, a), Bool)] -> a -> [a] -> [a] -> a -> [a]
subDepthFirst xs cmp trackxs outxs cmpbase = 
    let (newxs, newtrackxs, newcmp, isend, newoutxs) = case find (\((v1, v2), alrd) -> v1 == cmp && not alrd) xs of
                          Just ((src, tgt), _) -> (map (\((v1, v2), alrd) -> if v1 == cmp && v2 == tgt && not alrd || v2 == cmp && not alrd
                                                           then ((v1, v2), True)
                                                           else ((v1, v2), alrd)) xs, (trackxs ++ [cmp]), tgt, False, (outxs ++ [tgt]))
                          Nothing -> (map (\((v1, v2), alrd) -> if v2 == cmp && not alrd
                                                           then ((v1, v2), True)
                                                           else ((v1, v2), alrd)) xs, init trackxs, last trackxs, True, outxs) 
    in if null trackxs && isend
       then newoutxs
       else subDepthFirst newxs newcmp newtrackxs newoutxs cmpbase

-- 88

-- Connected components (alternative solution). 

-- Write a predicate that splits a graph into its connected components.
-- 
-- Example in Haskell:
-- 
-- λ> connectedComponents ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
-- [[1,2,3,4,5][6,7]]

graph88 = ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
graph88b = ([1,2,3,4,5,6,7], [(1,2),(1,3),(1,4),(6,7)])

connectedComponents :: (Eq a) => ([a], [(a, a)]) -> [[a]]
connectedComponents (nodexs, edgexs) = 
    let newedgexs = edgexs ++ map (\(x, y) -> (y, x)) edgexs 
    in subConnectedComponents (zip nodexs (replicate (length nodexs) False)) (zip newedgexs (replicate (length newedgexs) False))

subConnectedComponents :: (Eq a) => [(a, Bool)] -> [((a, a), Bool)] -> [[a]]
subConnectedComponents nodexs edgexs = case find (\(_, alrd) -> not alrd) nodexs of
                                           Just (x, _) -> 
                                               let (outxs, newedgexs) = subConnectedComponents2 [x] edgexs [x]
                                                   newnodexs = map (\(val, alrd) -> if val `elem` outxs
                                                                                 then (val, True)
                                                                                 else (val, alrd)) nodexs
                                               in outxs:(subConnectedComponents newnodexs newedgexs)
                                           Nothing -> []

subConnectedComponents2 :: (Eq a) => [a] -> [((a, a), Bool)] -> [a] -> ([a], [((a, a), Bool)])
subConnectedComponents2 outxs edgexs trackxs = case find (\((v1, v2), alrd) -> not alrd && v1 == head trackxs) edgexs of
                                            Just ((v1, v2), _) -> 
                                                let newedgexs = map (\((x1, x2), alrd) -> if x1 == v1 && x2 == v2 || x1 == v2 && x2 == v1
                                                                                then ((x1, x2), True)
                                                                                else ((x1, x2), alrd)) edgexs
                                                    newoutxs = if v2 `elem` outxs
                                                               then outxs
                                                               else (v2:outxs)
                                                in subConnectedComponents2 newoutxs newedgexs (v2:trackxs)
                                            Nothing -> if length trackxs == 1
                                                       then (outxs, edgexs)
                                                       else subConnectedComponents2 outxs edgexs (tail trackxs)
                                          
-- 89

-- Bipartite graphs. 

-- Write a predicate that finds out whether a given graph is bipartite.
-- 
-- Example in Haskell:
-- 
-- λ> bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
-- True
-- λ> bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])
-- False

graph89a = ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])

graph89b = ([1,2,3,4,5],[(1,2),(2,3),(1,3),(1,4),(3,4),(5,2),(5,4)])

graph89c = ([5,4,3,2,1],[(1,2),(2,3),(3,4),(5,4)])

bipartite :: (Eq a) => ([a], [(a, a)]) -> Bool
bipartite (nodexs, edgexs) = 
    let newnodexs = connectedComponents (nodexs, edgexs)
        graphxs   = constructGraph newnodexs edgexs
    in  subBipartite graphxs

constructGraph :: (Eq a) => [[a]] -> [(a, a)] -> [([a], [(a, a)])]
constructGraph [] _ = []
constructGraph (xs:xss) edgexs = (xs, filter (\(x, y) -> x `elem` xs || y `elem` xs) edgexs):constructGraph xss edgexs

subBipartite :: (Eq a) => [([a], [(a, a)])] -> Bool
subBipartite [] = True
subBipartite ((nodexs, edgexs):graphxs) = 
    let outval = if null edgexs
                 then True
                 else subBipartite2 nodexs [] [] edgexs
    in if outval
       then subBipartite graphxs
       else False

subBipartite2 :: (Eq a) => [a] -> [a] -> [a] -> [(a, a)] -> Bool
subBipartite2 [] _ _ _ = True
subBipartite2 (nodeval:nodexs) grp1 grp2 edgexs = 
    let outxs = filter (\(val1, val2) -> val1 == nodeval 
                        || val2 == nodeval) edgexs
        outxs2 = map (\(val1, val2) -> if val1 == nodeval
                                       then val2
                                       else val1) outxs
        outxs2b = filter (\x -> x `elem` grp1 || x `elem` grp2) outxs2
        (isvalid, newgrp1, newgrp2) = if null outxs2b && not (nodeval `elem` (grp1 ++ grp2))
                                      then (True, nodeval:grp1, grp2 ++ outxs2)
                                      else if (head outxs2b) `elem` grp1
                                      then ((all (\x -> not $ x `elem` grp2) outxs2b) && (not $ nodeval `elem` grp1)
                                      , (grp1 ++ outxs2)
                                      , (nodeval:grp2))
                                      else ((all (\x -> not $ x `elem` grp1) outxs2b) && (not $ nodeval `elem` grp2)
                                      , (nodeval:grp1)
                                      , (grp2 ++ outxs2))
    in if isvalid
       then subBipartite2 nodexs newgrp1 newgrp2 edgexs
       else False


-- 90

-- Eight queens problem. 

-- This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.
-- 
-- Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.
-- 
-- Example in Haskell:
-- 
-- λ> length (queens 8)
-- 92
-- λ> head (queens 8)
-- [1,5,8,6,3,7,2,4]

queens :: [[Int]]
queens = 
    let outxs = map (\[c, r] -> (c, r)) (sequence [[1..8], [1..8]])
    in subQueens (zip outxs (replicate 64 False)) []

subQueens :: [((Int, Int), Bool)] -> [Int] -> [[Int]]
subQueens xs outval =
    let col = length outval + 1
        outxs = filter (\((c, r), alrd) -> c == col && not alrd && not (r `elem` outval)) xs
    in if col > 8
       then [outval]
       else concat [ subQueens (updateChess c r xs) (outval ++ [r])
                   | ((c,r),_) <- outxs ]

updateChess :: Int -> Int -> [((Int, Int), Bool)] 
                -> [((Int, Int), Bool)]
updateChess c r xs =
    let newxs1 = map (\((v1, v2), alrd) -> if v1 == c || v2 == r
                                       then ((v1, v2), True)
                                       else ((v1, v2), alrd)) xs
        newxs2 = upperLeft newxs1 (c - 1) (r - 1)
        newxs3 = upperRight newxs2 (c + 1) (r - 1)
        newxs4 = lowerLeft newxs3 (c - 1) (r + 1)
        newxs5 = lowerRight newxs4 (c + 1) (r + 1)
    in newxs5

upperLeft :: [((Int, Int), Bool)] -> Int -> Int -> [((Int, Int), Bool)]
upperLeft xs 0 _ = xs
upperLeft xs _ 0 = xs
upperLeft xs c r = 
    let newxs = map (\((x, y), alrd) -> if x == c && y == r
                                           then ((x, y), True)
                                           else ((x, y), alrd)) xs
    in upperLeft newxs (c - 1) (r - 1)

upperRight :: [((Int, Int), Bool)] -> Int -> Int -> [((Int, Int), Bool)]
upperRight xs 9 _ = xs
upperRight xs _ 0 = xs
upperRight xs c r = 
    let newxs = map (\((x, y), alrd) -> if x == c && y == r
                                           then ((x, y), True)
                                           else ((x, y), alrd)) xs
    in upperRight newxs (c + 1) (r - 1)

lowerLeft :: [((Int, Int), Bool)] -> Int -> Int -> [((Int, Int), Bool)]
lowerLeft xs 0 _ = xs
lowerLeft xs _ 9 = xs
lowerLeft xs c r = 
    let newxs = map (\((x, y), alrd) -> if x == c && y == r
                                           then ((x, y), True)
                                           else ((x, y), alrd)) xs
    in lowerLeft newxs (c - 1) (r + 1)

lowerRight :: [((Int, Int), Bool)] -> Int -> Int -> [((Int, Int), Bool)]
lowerRight xs 9 _ = xs
lowerRight xs _ 9 = xs
lowerRight xs c r = 
    let newxs = map (\((x, y), alrd) -> if x == c && y == r
                                           then ((x, y), True)
                                           else ((x, y), alrd)) xs
    in lowerRight newxs (c + 1) (r + 1)

--testfunc :: Int -> [Int] -> [[Int]] -- yeaaaaaah map  acts like a concatenation
--testfunc 0 xs = if head xs == 10 
--                then []
--                else [xs]
--testfunc n xs = concat $ map (\x -> testfunc (n - 1) (x:xs)) [1..12]

-- 91

-- Knight's tour. 
-- Another famous problem is this one: How can a knight jump on an NxN chessboard in such a way that it visits every square exactly once? A set of solutions is given on the The_Knights_Tour page.
-- 
-- Hints: Represent the squares by pairs of their coordinates of the form X/Y, where both X and Y are integers between 1 and N. (Note that '/' is just a convenient functor, not division!) Define the relation jump(N,X/Y,U/V) to express the fact that a knight can jump from X/Y to U/V on a NxN chessboard. And finally, represent the solution of our problem as a list of N*N knight positions (the knight's tour).
-- 
-- There are two variants of this problem:
-- 
--     find a tour ending at a particular square
--     find a circular tour, ending a knight's jump from the start (clearly it doesn't matter where you start, so choose (1,1))
-- 
-- Example in Haskell:
-- 
-- λ> head $ knightsTo 8 (1,1)
-- [(2,7),(3,5),(5,6),(4,8),(3,6),(4,4),(6,5),(4,6),
-- (5,4),(7,5),(6,3),(5,5),(4,3),(2,4),(1,6),(2,8),
-- (4,7),(6,8),(8,7),(6,6),(4,5),(6,4),(5,2),(7,1),
-- (8,3),(6,2),(8,1),(7,3),(8,5),(7,7),(5,8),(3,7),
-- (1,8),(2,6),(3,4),(1,5),(2,3),(3,1),(1,2),(3,3),
-- (1,4),(2,2),(4,1),(5,3),(7,4),(8,2),(6,1),(4,2),
-- (2,1),(1,3),(2,5),(1,7),(3,8),(5,7),(7,8),(8,6),
-- (6,7),(8,8),(7,6),(8,4),(7,2),(5,1),(3,2),(1,1)]
-- λ> head $ closedKnights 8  
-- [(1,1),(3,2),(1,3),(2,1),(3,3),(5,4),(6,6),(4,5),
-- (2,6),(1,8),(3,7),(5,8),(4,6),(2,5),(4,4),(5,6),
-- (6,4),(8,5),(7,7),(6,5),(5,3),(6,1),(4,2),(6,3),
-- (8,2),(7,4),(5,5),(3,4),(1,5),(2,7),(4,8),(3,6),
-- (1,7),(3,8),(5,7),(7,8),(8,6),(6,7),(8,8),(7,6),
-- (8,4),(7,2),(5,1),(4,3),(3,5),(1,4),(2,2),(4,1),
-- (6,2),(8,1),(7,3),(5,2),(7,1),(8,3),(7,5),(8,7),
-- (6,8),(4,7),(2,8),(1,6),(2,4),(1,2),(3,1),(2,3)]


knightsTo :: (Int, Int) -> [(Int, Int)]
knightsTo (c, r) =
    let chessboard = zip (map (\[x, y] -> (x, y)) (sequence [[1..8], [1..8]])) (replicate 64 False)
        newchessboard = map (\((x1, x2), alrd) -> if x1 == c && x2 == r
                                        then ((x1, x2), True)
                                        else ((x1, x2), alrd)) chessboard
    in map (\(x, _) -> x) (subKnightsTo newchessboard [((c, r), 1)])

subKnightsTo :: [((Int, Int), Bool)] -> [((Int, Int), Int)] 
                -> [((Int, Int), Int)]
subKnightsTo _ [] = []
subKnightsTo chessboard (((c, r), nbmoove):posxs)
    | all (\(_, alrd) -> alrd) chessboard = posxs
    | otherwise = 
        let (newc, newr, isin) = forwardPos c r nbmoove
            blval = any (\((c1, r1), alrd) -> 
                c1 == newc && r1 == newr && alrd) chessboard
        in if blval || not isin
           then if nbmoove < 8
                then subKnightsTo chessboard (((c, r), nbmoove + 1):posxs)
                else 
                    let (newchessboard, newposxs) = trackBackPos chessboard (((c, r), nbmoove):posxs)
                    in subKnightsTo newchessboard newposxs
           else 
                let newposxs = ((newc, newr), 1):((c, r), nbmoove):posxs
                    newchessboard = map (\((x1, x2), alrd) -> if x1 == newc && x2 == newr
                                                      then ((x1, x2), True)
                                                      else ((x1, x2), alrd)) chessboard
                in subKnightsTo newchessboard newposxs

trackBackPos :: [((Int, Int), Bool)] -> [((Int, Int), Int)] 
                -> ([((Int, Int), Bool)], [((Int, Int), Int)])
trackBackPos chessboard (((c, r), nbmoove):posxs)
    | nbmoove == 8 = 
        let newchessboard = map (\((x1, x2), alrd) -> if x1 == c && x2 == r
                                                      then ((x1, x2), False)
                                                      else ((x1, x2), alrd)) chessboard
        in trackBackPos newchessboard posxs
    | otherwise = (chessboard, ((c, r), nbmoove + 1):posxs)

forwardPos :: Int -> Int -> Int -> (Int, Int, Bool)
forwardPos c r nbmoove = if nbmoove == 1
                         then mooveLeftUp c r 
                         else if nbmoove == 2
                         then mooveLeftDown c r
                         else if nbmoove == 3
                         then mooveUpLeft c r
                         else if nbmoove == 4
                         then mooveUpRight c r
                         else if nbmoove == 5
                         then mooveRightUp c r
                         else if nbmoove == 6
                         then mooveRightDown c r
                         else if nbmoove == 7
                         then mooveDownLeft c r
                         else if nbmoove == 8
                         then mooveDownRight c r
                         else (0, 0, False)

mooveLeftUp :: Int -> Int -> (Int, Int, Bool)
mooveLeftUp c r = if c - 2 < 1 || r - 1 < 1
                  then (0, 0, False)
                  else (c - 2, r - 1, True)
                  
mooveLeftDown :: Int -> Int -> (Int, Int, Bool)
mooveLeftDown c r = if c - 2 < 1 || r + 1 > 8
                    then (0, 0, False)
                    else (c - 2, r + 1, True)

mooveUpLeft :: Int -> Int -> (Int, Int, Bool)
mooveUpLeft c r = if c - 1 < 1 || r - 2 < 1
                  then (0, 0, False)
                  else (c - 1, r - 2, True)

mooveUpRight :: Int -> Int -> (Int, Int, Bool)
mooveUpRight c r = if c + 1 > 8 || r - 2 < 1
                   then (0, 0, False)
                   else (c + 1, r - 2, True)

mooveRightUp :: Int -> Int -> (Int, Int, Bool)
mooveRightUp c r = if c + 2 > 8 || r - 1 < 1
                   then (0, 0, False)
                   else (c + 2, r - 1, True)

mooveRightDown :: Int -> Int -> (Int, Int, Bool)
mooveRightDown c r = if c + 2 > 8 || r + 1 > 8
                     then (0, 0, False)
                     else (c + 2, r + 1, True)

mooveDownLeft :: Int -> Int -> (Int, Int, Bool)
mooveDownLeft c r = if c - 1 < 1 || r + 2 > 8
                    then (0, 0, False)
                    else (c - 1, r + 2, True)

mooveDownRight :: Int -> Int -> (Int, Int, Bool)
mooveDownRight c r = if c + 1 > 8 || r + 2 > 8
                    then (0, 0, False)
                    else (c + 1, r + 2, True)

betterKnightTo :: (Int, Int) -> [(Int, Int)]
betterKnightTo (c, r) = 
    let chessboard = zip (map (\[x, y] -> (x, y)) (sequence [[1..8], [1..8]])) (replicate 64 False)
        newchessboard = map (\((x, y), alrd) -> if x == c && y == r
                                           then ((x, y), True)
                                           else ((x, y), alrd)) chessboard
        moovexs = allMooves2 c r 1 chessboard [] []
    in map (\(x, _) -> x) (subBetterKnightToDFS newchessboard [((c, r), moovexs)])

subBetterKnightToDFS :: [((Int, Int), Bool)] -> [((Int, Int), [(Int, Int)])]
                     -> [((Int, Int), [(Int, Int)])]
subBetterKnightToDFS chessboard (((c, r), []):posxs) = 
    let newchessboard = map (\((x1, x2), alrd) -> if x1 == c && x2 == r
                                    then ((x1, x2), False)
                                    else ((x1, x2), alrd)) chessboard
    in subBetterKnightToDFS newchessboard posxs
subBetterKnightToDFS chessboard (((c, r), nextxs):posxs) =
    let (c2, r2) = head nextxs
        newchessboard = map (\((x1, x2), alrd) -> if x1 == c2 && x2 == r2
                                    then ((x1, x2), True)
                                    else ((x1, x2), alrd)) chessboard
        newnextxs = tail nextxs
        moovexs = allMooves2 c2 r2 1 chessboard [] []
    in if not . null $ moovexs
       then subBetterKnightToDFS newchessboard (((c2, r2), moovexs):((c, r), newnextxs):posxs)
       else if all (\(_, x) -> x) chessboard
       then (((c, r), newnextxs):posxs)
       else  subBetterKnightToDFS chessboard (((c, r), newnextxs):posxs)

--subBetterKnightToPure :: [((Int, Int), Bool)] -> [((Int, Int), [(Int, Int)])]
--                     -> [((Int, Int), [(Int, Int)])]
--subBetterKnightToPure chessboard (((c, r), []):posxs) = posxs
--subBetterKnightToPure chessboard (((c, r), nextxs):posxs) =
--    let (c2, r2) = head nextxs
--        newchessboard = map (\((x1, x2), alrd) ->
--                                if x1 == c2 && x2 == r2
--                                then ((x1, x2), True)
--                                else ((x1, x2), alrd)) chessboard
--        moovexs = allMooves2 c2 r2 1 chessboard [] []
--    in subBetterKnightToPure newchessboard (((c2, r2), moovexs):((c, r), tail nextxs):posxs)

allMooves2 :: Int -> Int -> Int -> [((Int, Int), Bool)]
              -> [Int] -> [(Int, Int)] -> [(Int, Int)]
allMooves2 _ _ 9 _ degxs posxs = 
    let outxs = sortAccordinglyAsc degxs posxs
    in outxs
allMooves2 c r n chessboard degxs posxs = 
    let (newc, newr, isin) = forwardPos c r n
        isin2 = if isin
                then filter (\((x1, x2), alrd) -> x1 == newc && x2 == newr && alrd) chessboard
                else []
    in if null isin2
       then 
           let val = allMooves newc newr 0
           in allMooves2 c r (n + 1) chessboard (val:degxs) ((newc, newr):posxs)
       else allMooves2 c r (n + 1) chessboard degxs posxs

allMooves :: Int -> Int -> Int -> Int
allMooves c r n
    | n > 8     = 0
    | otherwise =
        let (_, _, isvalid) = forwardPos c r n
        in (if isvalid then 1 else 0) + allMooves c r (n + 1)


type Pos = (Int, Int)
type Board = A.Array Pos Bool

-- Initialize 8x8 board with all False
initBoard :: Board
initBoard = A.array ((1,1), (8,8)) [((x,y), False) | x <- [1..8], y <- [1..8]]

-- Knight moves
knightMoves :: Pos -> [Pos]
knightMoves (c,r) = filter onBoard
    [ (c+2,r+1), (c+2,r-1), (c-2,r+1), (c-2,r-1)
    , (c+1,r+2), (c+1,r-2), (c-1,r+2), (c-1,r-2)
    ]
  where
    onBoard (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

-- Warnsdorff heuristic: sort moves by number of onward moves
sortByDegree :: Board -> [Pos] -> [Pos]
sortByDegree board = sortOn (\p -> length . filter (not . (board A.!)) $ knightMoves p)

-- DFS for knight's tour
knightDFS :: Board -> Pos -> [Pos] -> Maybe [Pos]
knightDFS board pos path
    | length path == 64 = Just (reverse path)  -- tour complete
    | otherwise =
        let board' = board A.// [(pos, True)]
            moves  = sortByDegree board' $ filter (not . (board' A.!)) (knightMoves pos)
        in tryMoves board' moves path
  where
    tryMoves _ [] _ = Nothing
    tryMoves b (m:ms) p =
        case knightDFS b m (m:p) of
            Just tour -> Just tour
            Nothing   -> knightDFS b (head ms) p 

-- Entry function
betterKnightTo2 :: Pos -> Maybe [Pos]
betterKnightTo2 start = knightDFS initBoard start [start]

testFunc :: Pos -> [(Int, Int)]
testFunc pxs = case betterKnightTo2 pxs of
                 Just x -> x
                 Nothing -> []

-- 93

-- unfinished, see https://julienlargetpiet.tech/all_posts/Unsolved_problem_!!!+1

-- operations order is purely from left to right

--puzzle :: [Int] -> [[Char]]
--puzzle (x:x2:xs) = 
--    filter (not . null) [puzzleFilter x x2 xs opers (show x2) | opers <- sequence (replicate l "+-*/")]
--    where l = length (xs)
--
--puzzleFilter :: Int -> Int -> [Int] -> [Char] -> [Char] -> [Char]
--puzzleFilter cmp val xs [] outxs = if val == cmp
--                                   then outxs ++ " = " ++ show val
--                                   else []
--puzzleFilter cmp val (x:xs) (op:operxs) outxs 
--    | op == '+' = puzzleFilter cmp (val + x) xs operxs (outxs ++ [' ', op, ' '] ++ show x)
--    | op == '-' = puzzleFilter cmp (val - x) xs operxs (outxs ++ [' ', op, ' '] ++ show x)
--    | op == '*' = puzzleFilter cmp (val * x) xs operxs (outxs ++ [' ', op, ' '] ++ show x)
--    | op == '/' && x /= 0 = puzzleFilter cmp (val `div` x) xs operxs (outxs ++ [' ', op, ' '] ++ show x)
--    | otherwise = []

-- Bonus question - in how many ways a list can be represented in a way that the sum of al its integers equal to n ?

howAdd :: Int -> [[Int]]
howAdd cmp = concat $ subHowAdd cmp 1

subHowAdd :: Int -> Int -> [[[Int]]]
subHowAdd cmp n
    | n < cmp = ((subHowAdd2 cmp n n [n]):subHowAdd cmp (n + 1))
    | otherwise = []

subHowAdd2 :: Int -> Int -> Int -> [Int] -> [[Int]]
subHowAdd2 _ 0 _ outxs = []
subHowAdd2 cmp n n2 outxs
    | n2 < cmp =  subHowAdd2 cmp n (n2 + n) (n:outxs) ++ 
                  subHowAdd2 cmp (n - 1) n2 outxs
    | n2 > cmp = subHowAdd2 cmp (n - 1) n2 outxs
    | otherwise = unique . permu $ outxs

-- 93, another approach that includes parenthesis order
          
data PTree a = PNode a [[PTree a]] deriving (Show, Eq)

howAddIntricated :: [[Int]] -> [[PTree Int]]
howAddIntricated []= []
howAddIntricated (xs:xss) = 
    let outxs = map (\x -> if x == 1
                   then PNode 1 []
                   else PNode x ((howAddIntricated (howAdd x)))) xs
    in [outxs] ++ howAddIntricated xss


howAddIntricated2 :: [[Int]] -> [[PTree Int]]
howAddIntricated2 [] = []
howAddIntricated2 (xs:xss) = 
    let outxs = foldl (\acc x -> if x == 1
                        then acc ++ [PNode 1 []]
                        else concatMap (\x2 -> 
                        acc ++ [PNode x (howAddIntricated2 [x2])]) (howAdd x)) [] xs
    in [outxs] ++ howAddIntricated2 xss

--[[PNode 1 [],PNode 1 [],PNode 1 [],PNode 1 []],
-- [PNode 2 [[PNode 1 [],PNode 1 []]],PNode 2 [[PNode 1 [],PNode 1 []]]],
-- [PNode 2 [[PNode 1 [],PNode 1 []]],PNode 1 [],PNode 1 []],
-- [PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]],PNode 1 []],
-- [PNode 1 [],PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]]],
-- [PNode 3 [[PNode 1 [],PNode 1 [],PNode 1 []]],PNode 3 [[PNode 2 [[PNode 1 [],PNode 1 []]],PNode 1 []]],PNode 3 [[PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]]]],PNode 1 []],
-- [PNode 1 [],PNode 3 [[PNode 1 [],PNode 1 [],PNode 1 []]],
-- PNode 1 [],PNode 3 [[PNode 2 [[PNode 1 [],PNode 1 []]],PNode 1 []]],
-- PNode 1 [],PNode 3 [[PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]]]]]]

testf :: [Int] -> [[Int]]
testf [] = []
testf (x:xs) = (map (\x -> replicate 5 x) [x]) ++ testf xs

testf2 :: [Int] -> [[Int]]
testf2 [] = []
testf2 (x:xs) = map (\x -> (replicate 5 x) ++ [0] ++ (concat $ testf2 xs)) [11, 15, 13]

howAddIntricated3 :: [[Int]] -> [PTree Int] -> [[PTree Int]] -> [[PTree Int]]
howAddIntricated3 ([]:xss) conf confset = if null $ conf
                                          then howAddIntricated3 xss [] confset
                                          else howAddIntricated3 xss [] (confset 
                                               ++ [conf])
howAddIntricated3 [] _ confset = confset
howAddIntricated3 (xs:xss) conf confset
    | all (== 1) xs = if null conf 
               then howAddIntricated3 ([]:xss) (replicate (length xs) (PNode 1 [])) confset
               else let [ptree] = conf
                        newptree = appendLastPTreen ptree (length xs)
                    in howAddIntricated3 ([]:xss) [newptree] confset
    | otherwise = if null conf 
                  then let (newx, ptree) = untilNotOne xs []
                       in map (\x2 -> concat $ (howAddIntricated3 [x2] (ptree 
                                                     ++ [PNode newx []]) []) ++
                       (howAddIntricated3 (xs:xss) conf confset)) (howAdd newx)
                  else let [ptree] = conf
                           (newx, ptree2) = untilNotOne xs []
                           newptree2 = if length ptree2 == 0
                                       then appendLastPTree ptree newx
                                       else let newptree = appendLastPTreen ptree (length ptree2)
                                            in appendLastPTree2 newptree newx
                       in map (\x2 -> concat $ (howAddIntricated3 [x2] [newptree2] []) ++
                       (howAddIntricated3 (xs:xss) [] confset)) (howAdd newx)

howAddIntricatedNotGood :: [Int] -> [[PTree Int]]
howAddIntricatedNotGood [] = [[]]
howAddIntricatedNotGood (x:xs)
    | all (== 1) (x:xs) = [replicate (length xs + 1) (PNode 1 [])]
    | otherwise = map (\x2 -> [PNode x [concat $ (howAddIntricatedNotGood x2) 
                                          ++ (howAddIntricatedNotGood xs)]]) (howAdd x)

untilNotOne :: [Int] -> [PTree Int] -> (Int, [PTree Int])
untilNotOne (x:xs) outxs
    | x == 1 = untilNotOne xs (outxs ++ [PNode 1 []])
    | otherwise = (x, outxs)

appendLastPTree :: PTree Int -> Int -> PTree Int
appendLastPTree (PNode vl []) x = PNode vl [[PNode x []]]
appendLastPTree (PNode vl restvl) x = PNode vl [[appendLastPTree (head . head $ restvl) x]]

appendLastPTree2 :: PTree Int -> Int -> PTree Int
appendLastPTree2 (PNode vl restvl) x = 
    let restvl2 = head restvl
    in if all (\(PNode x2 _) -> x2 == 1) restvl2
       then PNode vl (restvl ++ [[PNode x []]])
       else PNode vl [[appendLastPTree2 (head restvl2) x]]

appendLastPTreen :: PTree Int -> Int -> PTree Int
appendLastPTreen (PNode vl []) n = PNode vl [replicate n (PNode 1 [])]
appendLastPTreen (PNode vl restvl) n = PNode vl [[appendLastPTreen (head . head $ restvl) n]]

--[[PNode 3 [[PNode 1 [],PNode 1 [],PNode 1 []]]],
-- [PNode 3 [[PNode 2 [[PNode 1 [],PNode 1 []]]]]],
-- [PNode 1 []]]

testt2 :: PTree Int
testt2 = PNode 3 []

-- ideally we want a data structure like this, so a set of all possible configurations data, the configuration data are then properly used for creating the formulas that will be calculated with the `calc` function, but unfortunately the `howIntricated functions, gives a set of configurations data, but also, if node strictly superior to 3, again a set of configurations data, which is not intended`. We do not want intrication of configurations data, wa want a set of configurations data from which other functions can calculate directly

examplePTree :: PTree Int
examplePTree = PNode 4 [[PNode 1 [],PNode 1 [],PNode 1 [], PNode 1 []],
              [PNode 2 [[PNode 1 [], PNode 1 []]],PNode 1 [], PNode 1 []],
              [PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]], PNode 1 []],
              [PNode 1 [], PNode 1 [], PNode 2 [[PNode 1 [], PNode 1 []]]],
              [PNode 2 [[PNode 1 [], PNode 1 []]], PNode 2 [[PNode 1 [], PNode 1 []]]],
              [PNode 3 [[PNode 2 [[PNode 1 [], PNode 1 []]]], [PNode 1 []]], 
                        PNode 1 []],
              [PNode 3 [[PNode 1 [], PNode 1 [], PNode 1 []]], 
                        PNode 1 []],
              [PNode 3 [[PNode 1 []], [PNode 2 [[PNode 1 [], PNode 1 []]]]], 
                        PNode 1 []],
              [PNode 1 [], 
                  PNode 3 [[PNode 2 [[PNode 1 [], PNode 1 []]]], [PNode 1 []]]],
              [PNode 1 [], 
                  PNode 3 [[PNode 1 []], [PNode 2 [[PNode 1 [], PNode 1 []]]]]]]

subDividing :: [Int] -> [Int] -> Int -> [[Int]]
subDividing _ [] _ = []
subDividing ids (n:ns) n2 = (getRangeList ids (map (\x -> x + n2) [0..(n - 1)])):subDividing ids ns (n2 + n)

calculatePTree :: PTree Int -> [[Char]] -> [Int] -> [[Char]] -> [([[Char]], [[Char]])]
calculatePTree (PNode x ptrees) xs ns n = 
    let outids = subDividing ns (map (\vl -> sum (map (\(PNode vl2 _) -> vl2) vl)) ptrees) 0
    in concatMap (
                \(restxs, curids) ->
                  if all (\(PNode vl _) -> vl == 1) restxs
                  then [((getRangeList xs curids), n)]
                  else
                        let outids2 = subDividing curids (map (\(PNode vl _) -> vl) restxs) 0
                        in concatMap (\(ptree, val) -> 
                calculatePTree ptree xs val (n ++ [(foldl (\acc valb -> acc ++ show valb) "" val)])) (zip restxs outids2)
                ) (zip ptrees outids)

createFormula :: [[Char]] -> [Char] -> [Char]
createFormula (num:nums) ops = subCreateFormula nums ops num

subCreateFormula :: [[Char]] -> [Char] -> [Char] -> [Char]
subCreateFormula [] _ outxs = outxs
subCreateFormula _ [] outxs = outxs
subCreateFormula (num:nums) (op:ops) outxs = subCreateFormula nums ops (outxs ++ [op] ++ num)

updateOperators :: [([Char], [[Char]])] -> [Char] -> [Char] -> [Char]
updateOperators [] _ outops = outops
updateOperators _ [] outops = outops
updateOperators ((x, _):xs) (op:ops) outops =
    let nb = length $ grepmn2 "+-*/" x
    in if nb /= 0
       then 
           let newops = tailn (nb - 1) ops
           in if null newops
              then outops
              else updateOperators xs (tail newops) (outops ++ [head newops])
       else updateOperators xs ops (outops ++ [op])

--puzzle :: [Char] -> [[Char]] -> [Char] -> [[Char]]
--puzzle rslt nbs ops = 
--    let outhowadd = howAdd (length nbs)
--        refptree = howAddIntricated outhowadd
--    in concat [val1 | curops <- sequence (rep (length ops) ops)
--           , (val1, val2) <- subPuzzle nbs curops refptree,
--           val2 == rslt]

--realConfigurations :: [PTree Int] -> [[Int]]
--realConfigurations xs = 

subPuzzle :: [[Char]] -> [Char] -> (PTree Int) -> [([Char], [Char])]
subPuzzle nbs ops (PNode x restxs) =
    let outv = map (\xs -> let newxs = map (\valx -> [valx]) xs
                               outxs = calculatePTree (PNode x newxs) nbs [0..l] []
                               outxs2 = createFormula2 outxs ops
                               newops = updateOperators outxs2 ops []
                               newformula = evaluateFormula outxs2 newops
                           in (newformula, calc newformula)) restxs
    in outv
    where l = length nbs - 1

evaluateFormula :: [([Char], [[Char]])] -> [Char] -> [Char]
evaluateFormula xs ops
    | all (\(_, vl) -> null vl) xs = 
        let outv = map (\(vl, _) -> vl) xs
        in createFormula outv ops
    | otherwise = 
        let depthxs = map (\(_, l) -> length l) xs
            maxval = myMax depthxs
            idx = grep2 maxval depthxs
            (x, lst) = (xs !! idx)
            newlst = init lst
        in if null newlst
            then let newxs = updateListElem xs idx ("(" ++ x ++ ")", newlst)
                 in evaluateFormula newxs ops
            else case find (\((_, lval), _) -> lval == init lst) (zip xs [0..(length xs - 1)]) of
                          Just xout -> 
                              let ((x2, _), opidx) = xout
                                  (newx2, newops) = if opidx < idx
                                          then (x2 ++ [(ops !! opidx)] ++ "(" ++ x ++ ")"
                                                ,deleteListElem ops opidx)
                                          else ("(" ++ x ++ ")" ++ [(ops !! idx)] ++ x2
                                                ,deleteListElem ops idx)
                                  newxs = updateListElem xs opidx (newx2, newlst)
                                  newxs2 = deleteListElem newxs idx
                              in evaluateFormula newxs2 newops
                          Nothing -> ""
        
createFormula2 :: [([[Char]], [[Char]])] -> [Char] -> [([Char], [[Char]])]
createFormula2 [] _ = []
createFormula2 [(x1, x2)] [] = [(concat x1, x2)]
createFormula2 ((x, n):xs) (op:ops)
    | length x == 1 = [((x !! 0), n)] ++ createFormula2 xs ops
    | otherwise     = 
        let (newx, newops) = subCreateFormula2 (op:ops) x
        in [(newx, n)] ++ createFormula2 xs (tail newops)

subCreateFormula2 :: [Char] -> [[Char]] -> ([Char], [Char])
subCreateFormula2 ops (x:xs) = subCreateFormula2b ops xs x

subCreateFormula2b :: [Char] -> [[Char]] -> [Char] -> ([Char], [Char])
subCreateFormula2b ops [] outxs = (outxs, ops)
subCreateFormula2b (op:ops) (x:xs) outxs = subCreateFormula2b ops xs (outxs ++ [op] ++ x)













