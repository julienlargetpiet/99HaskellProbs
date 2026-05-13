import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Char (isDigit, isSpace)
import Control.DeepSeq (deepseq)

data Token
    = TNum Double
    | TPlus
    | TMinus
    | TMul
    | TDiv
    | TPow
    | TExp
    | TLog
    | TLParen
    | TRParen
    deriving (Show, Eq)

tokenize :: ByteString -> Either String [Token]
tokenize bs0 =
    let bs = B.dropWhile isSpace bs0
    in if B.null bs
       then Right []
       else if B.isPrefixOf (B.pack "exp") bs
            then (TExp :) <$> tokenize (B.drop 3 bs)
       else if B.isPrefixOf (B.pack "log") bs
            then (TLog :) <$> tokenize (B.drop 3 bs)
       else
            case B.uncons bs of
                Nothing ->
                    Right []

                Just (x, xs)
                    | isDigit x || x == '.' ->
                        let (numTxt, rest) =
                                B.span (\c -> isDigit c || c == '.') bs
                        in case reads (B.unpack numTxt) :: [(Double, String)] of
                            [(n, "")] -> --parser succeeds
                                (TNum n :) <$> tokenize rest

                            _ ->
                                Left ("Invalid number: " ++ B.unpack numTxt)

                    | x == '+' ->
                        (TPlus :) <$> tokenize xs

                    | x == '-' ->
                        (TMinus :) <$> tokenize xs

                    | x == '*' ->
                        (TMul :) <$> tokenize xs

                    | x == '/' ->
                        (TDiv :) <$> tokenize xs

                    | x == '^' ->
                        (TPow :) <$> tokenize xs

                    | x == 'E' ->
                        (TExp :) <$> tokenize xs

                    | x == 'L' ->
                        (TLog :) <$> tokenize xs

                    | x == '(' ->
                        (TLParen :) <$> tokenize xs

                    | x == ')' ->
                        (TRParen :) <$> tokenize xs

                    | otherwise ->
                        Left ("Unknown character: " ++ [x])

-- Addition & Substraction

parseExpr :: [Token] -> Either String (Double, [Token])
parseExpr tokens = do
    (lhs, rest) <- parseTerm tokens
    parseExprRest lhs rest

parseExprRest :: Double -> [Token] -> Either String (Double, [Token])
parseExprRest acc tokens =
    case tokens of
        TPlus : rest -> do
            (rhs, rest') <- parseTerm rest
            parseExprRest (acc + rhs) rest'

        TMinus : rest -> do
            (rhs, rest') <- parseTerm rest
            parseExprRest (acc - rhs) rest'

        _ ->
            Right (acc, tokens)
---------

-- Multiplication & Division

parseTerm :: [Token] -> Either String (Double, [Token])
parseTerm tokens = do
    (lhs, rest) <- parsePower tokens
    parseTermRest lhs rest

parseTermRest :: Double -> [Token] -> Either String (Double, [Token])
parseTermRest acc tokens =
    case tokens of
        TMul : rest -> do
            (rhs, rest') <- parsePower rest
            parseTermRest (acc * rhs) rest'

        TDiv : rest -> do
            (rhs, rest') <- parsePower rest
            parseTermRest (acc / rhs) rest'

        _ ->
            Right (acc, tokens)

------

-- Exponentiation

parsePower :: [Token] -> Either String (Double, [Token])
parsePower tokens = do
    (base, rest) <- parseUnary tokens
    case rest of
        TPow : rest' -> do
            (exponent, rest'') <- parsePower rest'
            Right (base ** exponent, rest'')

        _ ->
            Right (base, rest)


-----

-- Identity

parseUnary :: [Token] -> Either String (Double, [Token])
parseUnary tokens =
    case tokens of
        TPlus : rest ->
            parseUnary rest

        TMinus : rest -> do -- impressive, very nice
            (v, rest') <- parseUnary rest
            Right (-v, rest')

        TExp : rest -> do
            (v, rest') <- parseUnary rest
            Right (exp v, rest')

        TLog : rest -> do
            (v, rest') <- parseUnary rest
            Right (log v, rest')

        _ ->
            parsePrimary tokens

-----------

parsePrimary :: [Token] -> Either String (Double, [Token])
parsePrimary tokens =
    case tokens of
        TNum n : rest ->
            Right (n, rest)

        TLParen : rest -> do
            (v, rest') <- parseExpr rest
            case rest' of
                TRParen : rest'' ->
                    Right (v, rest'')

                _ ->
                    Left "Expected closing parenthesis"

        [] ->
            Left "Unexpected end of expression"

        tok : _ ->
            Left ("Unexpected token: " ++ show tok)

----

--- calc

calc :: ByteString -> Either String Double
calc input = do
    tokens <- tokenize input
    parseCalc tokens

parseCalc :: [Token] -> Either String Double
parseCalc tokens =
    case parseExpr tokens of
        Left err ->
            Left err

        Right (result, rest) ->
            case rest of
                [] ->
                    Right result

                _ ->
                    Left ("Unexpected tokens at end: " ++ show rest)

benchCalc :: Int -> ByteString -> Either String Double
benchCalc 1 expr = calc expr
benchCalc n expr =
    let r = calc expr
    in r `deepseq` benchCalc (n - 1) expr

main :: IO ()
main = do
    let expr = B.pack "-6+-(-7+E-3/0.2)*4"
    let result = benchCalc 100000 expr
    case result of

        Left err ->
            putStrLn ("Error:" ++ err)

        Right value ->
            print value




