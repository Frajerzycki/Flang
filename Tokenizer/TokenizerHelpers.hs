module Tokenizer.TokenizerHelpers
        (boolMatchRegex, keyword, boolLiteral, first,
        second, third, nextInteger, unescapeChar)
        where
        import Data.Char
        import Text.Regex
        import Tokenizer.ErrorLogging(logError)


        boolMatchRegex :: Maybe [String] -> Bool
        boolMatchRegex (Just _) = True
        boolMatchRegex Nothing  = False

        keyword :: Regex
        keyword = mkRegex "^(if|else|data|int|integer|boolean|string|char)$"

        boolLiteral :: Regex
        boolLiteral = mkRegex "^(true|false)$"


        first :: (a, b, c)  -> a
        first (s, _, _) = s

        second :: (a, b, c) -> b
        second (_, i, _) = i

        third :: (a, b, c)  -> c
        third (_, _, s) = s



        nextInteger :: String -> Int -> String -> (String, Int, String)
        nextInteger [] p result = ([], p, result)
        nextInteger (x:xs) p result
                | isDigit x = nextInteger xs  (p + 1) (result ++ [x])
                | otherwise = ((x:xs), (p + 1), result)


        unescapeChar :: String-> Int -> Int -> (Char, String, Int)
        unescapeChar [] p1 p2 = logError "There isn't any char at " p1 p2
        unescapeChar (x:xs) p1 p2 =
                let
                        intToUnescape           = nextInteger (x:xs) p2 []
                        unescapedChar             = case x of
                                't' -> '\t'
                                'b' -> '\b'
                                'n' -> '\n'
                                'r' -> '\r'
                                'f' -> '\f'
                                '\'' -> x
                                '\"' -> x
                                '\\' -> x
                                _    -> case isDigit x of
                                        True    -> chr . read $ third
                                                intToUnescape
                                        False -> logError
                                                "Unknown escape char at " p1 p2
                in (unescapedChar, first intToUnescape, second intToUnescape)
