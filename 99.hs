import qualified System.Random as R

--1
myLast :: [a] -> a
myLast xs = last xs

--1
myButLast :: [a] -> a
myButLast (x1:x2:x3:xs) = x3

myButLast2 :: [a] -> a
myButLast2 xs
    | length xs > 1 = (xs !! (length xs - 2))
    | otherwise = head xs


--3
elemAt :: [a] -> Int -> Maybe a
elemAt xs idx
    | idx > length xs = Nothing
    | otherwise = Just (xs !! (idx - 1))


--4
myLength :: [a] -> Int
myLength xs = length xs


--5
myReverse :: [a] -> [a]
myReverse xs = reverse xs

myReverse2 :: [a] -> [a]
myReverse2 [] = []
myReverse2 xs = (last xs):(myReverse2 (init xs))

--6
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
data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat (fmap flatten xs)

--8
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

duppli :: [a] -> [a]
duppli [] = []
duppli (x:xs) = x:x:(duppli xs)

-- 15

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) hmn = (subRepli x hmn) ++ (repli xs hmn)

subRepli :: a -> Int -> [a]
subRepli _ 0 = []
subRepli x hmn = x:(subRepli x (hmn - 1))

-- 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs idx = subDropEvery xs idx 1

subDropEvery :: [a] -> Int -> Int -> [a]
subDropEvery [] _ _ = []
subDropEvery (x:xs) idx idx2
    | idx /= idx2 = x:(subDropEvery xs idx (idx2 + 1))
    | otherwise = subDropEvery xs idx 1


-- 17 one of the hardes i find

split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) n =
    let (xs1, xs2) = split xs (n - 1)
    in (x:xs1, xs2) -- the x:xs will be applied just before return

-- 18 broooo, that is alredy done with getRangeList

-- 19

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

removeAt :: Int -> [a] -> (a, [a])
removeAt idx xs = ((xs !! (idx - 1)), subRemoveAt idx 1 xs)

subRemoveAt :: Int -> Int -> [a] -> [a]
subRemoveAt _ _ [] = []
subRemoveAt idx curidx (x:xs)
    | idx /= curidx = x:(subRemoveAt idx (curidx + 1) xs)
    | otherwise = subRemoveAt idx (curidx + 1) xs



-- 21

insertAt :: a -> [a] -> Int -> [a]
insertAt insrt xs idx = 
    let (xs1, xs2) = split xs (idx - 1)
    in xs1 ++ [insrt] ++ xs2

-- 22

range :: Int -> Int -> [Int]
range x1 x2
    | x1 < x2 = x1:(range (x1 + 1) x2)
    | otherwise = x1:[]

--23

rnd_select :: [a] -> Int -> [a]
rnd_select xs n = subRnd_Select xs n 0 (R.mkStdGen $ length xs) (length xs)

subRnd_Select :: [a] -> Int -> Int -> R.StdGen -> Int -> [a]
subRnd_Select xs n n2 gen l
    | n2 < n =
        let (val, newgen) = R.random gen
        in (xs !! ((abs val) `mod` l)):(subRnd_Select xs n (n2 + 1) newgen l)
    | otherwise = []


-- 24

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

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k - 1) xs) ++ combinations k xs


-- 27 

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

myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

-- 33

coprime :: Int -> Int -> Bool
coprime a b = (myGCD a b) == 1

-- 34

totient :: Int -> Int
totient x = subTotient x x 1 

subTotient :: Int -> Int -> Int -> Int
subTotient 1 cmp n = n
subTotient x cmp n
    | coprime x cmp = subTotient (x - 1) cmp (n + 1)
    | otherwise = subTotient (x - 1) cmp n


-- 35

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

totient2 :: Int -> Int
totient2 x = mult [(p - 1) * p ^ (m -1) | (p, m) <- primeFactorsMult x]

mult :: (Num a) => [a] -> a
mult [] = 1
mult (x:xs) = x * (mult xs)


-- 38 
-- do it on your own


-- 39

primesR :: Int -> Int -> [Int]
primesR a b
    | a /= b = if isPrime a
               then (a:(primesR (a + 1) b))
               else primesR (a + 1) b
    | otherwise = []

-- 40

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

--Already done

-- 48

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

gray :: Int -> [[Char]]
gray n = 
    let xs = myReplicate n "10"
    in mySequence xs


-- 50

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

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b) deriving (Show)

myTrees :: [Tree Int Char]
myTrees = [
          Node 34  (Leaf 'A') (Leaf 'A'), 
          Node 121 (Leaf 'A') (Leaf 'A'),
          Node 21  (Leaf 'A') (Leaf 'A'),
          Node 12  (Leaf 'A') (Leaf 'A'),
          Node 65  (Leaf 'A') (Leaf 'A'),
          Node 6   (Leaf 'A') (Leaf 'A')
          ]
        
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








