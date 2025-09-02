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

















