module Tokenizer.TokenizerHelpers
        (getPosition, boolMatchRegex, keyword, boolLiteral, first,
        second, third, nextInteger, Token (..), Tokens)
        where
        import Data.Char
        import Text.Regex



        getPosition :: Int -> Int -> String
        getPosition p1 p2 = (show p1) ++ ":" ++ (show p2)

        boolMatchRegex :: Maybe [String] -> Bool
        boolMatchRegex (Just _) = True
        boolMatchRegex Nothing  = False

        keyword :: Regex
        keyword = mkRegex "^(if|else|data|int|integer|boolean|string|char)$"

        boolLiteral :: Regex
        boolLiteral = mkRegex "^(true|false)$"


        first :: (a,c,a) -> a
        first (s, _, _) = s

        second :: (a,c,a) -> c
        second (_, i, _) = i

        third :: (a, c, a) -> a
        third (_, _, s) = s



        nextInteger :: String -> Int -> String -> (String, Int, String)
        nextInteger [] p result = ([], p, result)
        nextInteger (x:xs) p result
                | isDigit x = nextInteger xs  (p + 1) (result ++ [x])
                | otherwise = ((x:xs), (p + 1), result)

        data Token = Keyword String Int Int
                | Operator String Int Int
                | Identifier String Int Int
                | RangeOperator Char Int Int
                | IntegerLiteral String Int Int
                | FloatingLiteral String Int Int
                | StringLiteral String Int Int
                | CharLiteral Char Int Int
                | BooleanLiteral String Int Int deriving (Show)
        type Tokens = [Token]
