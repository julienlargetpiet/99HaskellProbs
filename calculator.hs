import Text.Printf
import Debug.Trace

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

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
        idstrt = ids !! id2
        idstop = ids !! id1

        xsstrt = if idstrt > 0
                 then getRangeList xs [0..(idstrt - 1)]
                 else []

        xsstop = if idstop + 1 < length xs
                 then getRangeList xs [(idstop + 1)..(length xs - 1)]
                 else []

        xsbetween = getRangeList xs [(idstrt + 1)..(idstop - 1)]
        rslt = protoCalc xsbetween

        newxs = xsstrt ++ rslt ++ xsstop

        (newids, newnums) = parserPar newxs
    in subCalc newxs newids newnums

--protoCalc :: [Char] -> [Char]
--protoCalc xs =
--    let step0 = clearOperator xs
--
--        step1 = subProtoCalcIdentity step0 []
--        step2 = clearOperator step1
--
--        step3 = subProtoCalcExponent step2 []
--        step4 = clearOperator step3
--
--        step5 = subProtoCalc step4 []
--        step6 = clearOperator step5
--
--        step7 = subProtoCalc2 step6 [] 0
--    in trace ("step0 input:    " ++ step0) $
--       trace ("step1 identity: " ++ step1) $
--       trace ("step2 clear:    " ++ step2) $
--       trace ("step3 exponent: " ++ step3) $
--       trace ("step4 clear:    " ++ step4) $
--       trace ("step5 */:       " ++ step5) $
--       trace ("step6 clear:    " ++ step6) $
--       trace ("step7 +-:       " ++ step7) $
--       clearOperator step7

protoCalc :: [Char] -> [Char]
protoCalc xs =
    let step0 = clearOperator xs

        step1 = subProtoCalcIdentity step0 []
        step2 = clearOperator step1

        step3 = subProtoCalcExponent step2 []
        step4 = clearOperator step3

        step5 = subProtoCalc step4 []
        step6 = clearOperator step5

        step7 = subProtoCalc2 step6 [] 0
    in clearOperator step7

takeBack2 :: [Char] -> Int -> [Char]
takeBack2 [] _ = []
takeBack2 (x:xs) n 
    | not (x `elem` "+-*/") = (x:takeBack2 xs (n+1))
    | otherwise = if n == 0 then (x:takeBack2 xs (n+1)) else []

takeTailN2 :: [Char] -> Int -> [Char]
takeTailN2 [] _ = []
takeTailN2 (x:xs) n
    | not (x `elem` "+-*/") = takeTailN2 xs (n+1)
    | otherwise = if n == 0 then takeTailN2 xs (n+1) else  x:xs

subProtoCalc :: [Char] -> [Char] -> [Char]
subProtoCalc [] outxs = outxs
subProtoCalc (x:xs) outxs
    | x == '*' =
            let val1 = read . reverse $ takeBack2 (reverse outxs) 0 :: Double
                val2 = read $ takeBack2 xs 0 :: Double
                newoutxs = reverse $ takeTailN2 (reverse outxs) 0
                newxs = takeTailN2 xs 0
            in subProtoCalc newxs (newoutxs ++ show (val1 * val2))

    | x == '/' =
            let val1 = read . reverse $ takeBack2 (reverse outxs) 0 :: Double
                val2 = read $ takeBack2 xs 0 :: Double
                newoutxs = reverse $ takeTailN2 (reverse outxs) 0
                newxs = takeTailN2 xs 0
            in subProtoCalc newxs (newoutxs ++ show (val1 / val2))

    | otherwise =
            subProtoCalc xs (outxs ++ [x])

clearOperator :: [Char] -> [Char]
clearOperator [] = []
clearOperator [x] = [x]
clearOperator (x1:x2:xs)
    | x1 == '+' && x2 == '-' = clearOperator ('-':xs)
    | x1 == '-' && x2 == '+' = clearOperator ('-':xs)
    | x1 == '+' && x2 == '+' = clearOperator ('+':xs)
    | x1 == '-' && x2 == '-' = clearOperator ('+':xs)
    | otherwise = x1 : clearOperator (x2:xs)

subProtoCalc2 :: [Char] -> [Char] -> Int -> [Char]
subProtoCalc2 [] outxs _ = outxs
subProtoCalc2 (x:xs) outxs n
    | x == '+' =
            let val1raw = read . reverse $ takeBack2 (reverse outxs) 0 :: Double
                val2 = read $ takeBack2 xs 0 :: Double

                newoutxsRaw = reverse $ takeTailN2 (reverse outxs) 0

                (newoutxs, val1) =
                    if newoutxsRaw == "-"
                    then ("", -val1raw)
                    else (newoutxsRaw, val1raw)

                newxs = takeTailN2 xs 0

            in subProtoCalc2 newxs (newoutxs ++ show (val1 + val2)) (n + 1)

    | x == '-' && n /= 0 =
            let val1raw = read . reverse $ takeBack2 (reverse outxs) 0 :: Double
                val2 = read $ takeBack2 xs 0 :: Double

                newoutxsRaw = reverse $ takeTailN2 (reverse outxs) 0

                (newoutxs, val1) =
                    if newoutxsRaw == "-"
                    then ("", -val1raw)
                    else (newoutxsRaw, val1raw)

                newxs = takeTailN2 xs 0

            in subProtoCalc2 newxs (newoutxs ++ show (val1 - val2)) (n + 1)

    | otherwise =
            subProtoCalc2 xs (outxs ++ [x]) (n + 1)

subProtoCalcIdentity :: [Char] -> [Char] -> [Char]
subProtoCalcIdentity [] outxs = outxs
subProtoCalcIdentity (x:xs) outxs
    | x == 'E' = 
        let val = read $ takeBack2 xs 0 :: Double
            newxs = takeTailN2 xs 0
        in subProtoCalcIdentity newxs (outxs ++ printf "%8f" ((exp(val)) :: Double) :: String)
    | x == 'L' = 
        let val = read $ takeBack2 xs 0 :: Double
            newxs = takeTailN2 xs 0
        in subProtoCalcIdentity newxs (outxs ++ printf "%8f" ((log(val)) :: Double) :: String)
    | x == '!' = 
        let val = read $ takeBack2 xs 0 :: Int
            newxs = takeTailN2 xs 0
        in subProtoCalcIdentity newxs (outxs ++ show (factorial val))
    | otherwise = subProtoCalcIdentity xs (outxs ++ [x])

subProtoCalcExponent :: [Char] -> [Char] -> [Char]
subProtoCalcExponent [] outxs = outxs
subProtoCalcExponent (x:xs) outxs
    | x == '^' = 
        let val1 = read . reverse $ takeBack2 (reverse outxs) 0 :: Double
            val2 = read $ takeBack2 xs 0 :: Double
            newoutxs = reverse $ takeTailN2 (reverse outxs) 0
            newxs = takeTailN2 xs 0 
        in subProtoCalcExponent newxs (newoutxs ++ (show (val1**(val2))))
    | otherwise = subProtoCalcExponent xs (outxs ++ [x])


benchCalc :: Int -> String -> String
benchCalc 1 expr = calc expr
benchCalc n expr =
    let r = calc expr
    in r `seq` benchCalc (n - 1) expr

main :: IO ()
main = do
    let expr = "-6+-(-7+E-3/0.2)*4"
    let result = benchCalc 100000 expr
    putStrLn result




