import Text.Printf
import Debug.Trace

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import Control.DeepSeq (deepseq)

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

grepn2 :: (Eq a) => a -> [a] -> [Int]
grepn2 cmp xs = subGrepn2 xs cmp 0 []

subGrepn2 :: (Eq a) => [a] -> a -> Int -> [Int] -> [Int]
subGrepn2 [] _ _ nxs = nxs
subGrepn2 (x:xs) cmp n nxs
    | cmp == x  = subGrepn2 xs cmp (n + 1) (n:nxs)
    | otherwise = subGrepn2 xs cmp (n + 1) nxs

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

calc :: ByteString -> ByteString
calc xs = 
    let (ids, nums) = parserPar xs
        newxs = subCalc xs ids nums
    in protoCalc newxs
    
subCalc :: ByteString -> [Int] -> [Int] -> ByteString
subCalc xs [] [] = xs
subCalc xs ids nums =
    let curmax = myMax nums
        [id1, id2] = grepn2 curmax nums

        idstrt = ids !! id2
        idstop = ids !! id1

        xsstrt = B.take idstrt xs
        xsstop = B.drop (idstop + 1) xs
        xsbetween = B.take (idstop - idstrt - 1) (B.drop (idstrt + 1) xs)

        rslt = protoCalc xsbetween

        newxs = xsstrt <> rslt <> xsstop

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

protoCalc :: ByteString -> ByteString
protoCalc xs =
    let step0 = clearOperator xs

        step1 = subProtoCalcIdentity step0 B.empty
        step2 = clearOperator step1

        step3 = subProtoCalcExponent step2 B.empty
        step4 = clearOperator step3

        step5 = subProtoCalc step4 B.empty
        step6 = clearOperator step5

        step7 = subProtoCalc2 step6 B.empty 0
    in clearOperator step7

takeBack2 :: ByteString -> Int -> ByteString
takeBack2 bs n = 
    case B.uncons bs of
        
        Nothing -> B.empty

        Just (x, xs) 
            | not (x `elem` "+-*/^") -> B.cons x (takeBack2 xs (n+1))
            | otherwise -> if n == 0 then B.cons x (takeBack2 xs (n+1)) else B.empty

takeTailN2 :: ByteString -> Int -> ByteString
takeTailN2 bs n = 
    case B.uncons bs of
        
        Nothing -> B.empty

        Just (x, xs)
            | not (x `elem` "+-*/^") -> takeTailN2 xs (n+1)
            | otherwise -> if n == 0 then takeTailN2 xs (n+1) else B.cons x xs

subProtoCalc :: B.ByteString -> B.ByteString -> B.ByteString
subProtoCalc bs outxs =
    case B.uncons bs of

        Nothing ->
            outxs

        Just (x, xs)
            | x == '*' ->
                let val1 =
                        read . B.unpack . B.reverse $
                            takeBack2 (B.reverse outxs) 0 :: Double

                    val2 =
                        read . B.unpack $
                            takeBack2 xs 0 :: Double

                    newoutxs =
                        B.reverse $ takeTailN2 (B.reverse outxs) 0

                    newxs =
                        takeTailN2 xs 0

                    result =
                        B.pack (show (val1 * val2))

                in subProtoCalc newxs (newoutxs <> result)

            | x == '/' ->
                let val1 =
                        read . B.unpack . B.reverse $
                            takeBack2 (B.reverse outxs) 0 :: Double

                    val2 =
                        read . B.unpack $
                            takeBack2 xs 0 :: Double

                    newoutxs =
                        B.reverse $ takeTailN2 (B.reverse outxs) 0

                    newxs =
                        takeTailN2 xs 0

                    result =
                        B.pack (show (val1 / val2))

                in subProtoCalc newxs (newoutxs <> result)

            | otherwise ->
                subProtoCalc xs (B.snoc outxs x)

clearOperator :: B.ByteString -> B.ByteString
clearOperator bs =
    case B.uncons bs of
        Nothing ->
            B.empty

        Just (x1, rest) ->
            case B.uncons rest of
                Nothing ->
                    B.singleton x1

                Just (x2, xs)
                    | x1 == '+' && x2 == '-' ->
                        clearOperator (B.cons '-' xs)

                    | x1 == '-' && x2 == '+' ->
                        clearOperator (B.cons '-' xs)

                    | x1 == '+' && x2 == '+' ->
                        clearOperator (B.cons '+' xs)

                    | x1 == '-' && x2 == '-' ->
                        clearOperator (B.cons '+' xs)

                    | otherwise ->
                        B.cons x1 (clearOperator (B.cons x2 xs))

subProtoCalc2 :: ByteString -> ByteString -> Int -> ByteString
subProtoCalc2 bs outxs n =
    case B.uncons bs of
        Nothing ->
            outxs

        Just (x, xs)
            | x == '+' ->
                let val1raw =
                        read . B.unpack . B.reverse $
                            takeBack2 (B.reverse outxs) 0 :: Double

                    val2 =
                        read . B.unpack $
                            takeBack2 xs 0 :: Double

                    newoutxsRaw =
                        B.reverse $ takeTailN2 (B.reverse outxs) 0

                    (newoutxs, val1) =
                        if newoutxsRaw == B.singleton '-'
                        then (B.empty, -val1raw)
                        else (newoutxsRaw, val1raw)

                    newxs =
                        takeTailN2 xs 0

                    result =
                        B.pack (show (val1 + val2))

                in subProtoCalc2 newxs (newoutxs <> result) (n + 1)

            | x == '-' && n /= 0 ->
                let val1raw =
                        read . B.unpack . B.reverse $
                            takeBack2 (B.reverse outxs) 0 :: Double

                    val2 =
                        read . B.unpack $
                            takeBack2 xs 0 :: Double

                    newoutxsRaw =
                        B.reverse $ takeTailN2 (B.reverse outxs) 0

                    (newoutxs, val1) =
                        if newoutxsRaw == B.singleton '-'
                        then (B.empty, -val1raw)
                        else (newoutxsRaw, val1raw)

                    newxs =
                        takeTailN2 xs 0

                    result =
                        B.pack (show (val1 - val2))

                in subProtoCalc2 newxs (newoutxs <> result) (n + 1)

            | otherwise ->
                subProtoCalc2 xs (B.snoc outxs x) (n + 1)

subProtoCalcIdentity :: ByteString -> ByteString -> ByteString
subProtoCalcIdentity bs outxs = 
    case B.uncons bs of

        Nothing -> outxs

        Just(x, xs)
            | x == 'E' -> 
                let val = read . B.unpack $ takeBack2 xs 0 :: Double
                    newxs = takeTailN2 xs 0
                    result = B.pack (printf "%8f" (exp val) :: String)
                in subProtoCalcIdentity newxs (outxs <> result)
            | x == 'L' -> 
                let val = read . B.unpack $ takeBack2 xs 0 :: Double
                    newxs = takeTailN2 xs 0
                    result = B.pack (printf "%8f" (log val) :: String)
                in subProtoCalcIdentity newxs (outxs <> result)
            | x == '!' ->
                let val = read . B.unpack $ takeBack2 xs 0 :: Int
                    newxs = takeTailN2 xs 0
                    result = B.pack $ show (factorial val)
                in subProtoCalcIdentity newxs (outxs <> result)
            | otherwise -> subProtoCalcIdentity xs (B.snoc outxs x)


subProtoCalcExponent :: ByteString -> ByteString -> ByteString
subProtoCalcExponent bs outxs = 
    case B.uncons bs of

        Nothing -> outxs

        Just(x, xs)
            | x == '^' ->
                let val1 = read . B.unpack . B.reverse $ takeBack2 (B.reverse outxs) 0 :: Double
                    val2 = read . B.unpack $ takeBack2 xs 0 :: Double
                    newoutxs = B.reverse $ takeTailN2 (B.reverse outxs) 0
                    newxs = takeTailN2 xs 0 
                    result = B.pack (show (val1**(val2)))
                in subProtoCalcExponent newxs (newoutxs <> result)
            | otherwise -> subProtoCalcExponent xs (B.snoc outxs x)

benchCalc :: Int -> ByteString -> ByteString
benchCalc 1 expr = calc expr
benchCalc n expr =
    let r = calc expr
    in r `deepseq` benchCalc (n - 1) expr

main :: IO ()
main = do
    let expr = B.pack "-6+-(-7+E-3/0.2)*4"
    let result = benchCalc 100000 expr
    B.putStrLn result


