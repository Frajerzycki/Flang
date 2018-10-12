module Tokenizer (Token, tokenize) where
        import Data.Char
        import Text.Regex
        import TokenizerHelpers

        -- Tokenizing
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


        tokenize :: String -> Tokens -> Int -> Int -> Tokens
        tokenize [] tokens _ _ = tokens
        tokenize (x:xs) tokens p1 p2
                | x == '\t'  || x == ':'        = rangeOperator
                | x == '\n'                     = newLine
                | x == ' '                      = space
                | isAlpha x || x == '_'         = tokenizeKeyword xs tokens p1
                        (p2 + 1) [x]
                | x == '\"'                     = tokenizeStringLiteral xs
                        tokens p1 p2
                        False False []
                where
                        rangeOperator   = tokenize xs
                                (tokens ++ [RangeOperator x p1 (p1 + 1)]) p1
                                        (p2 + 1)
                        newLine         = tokenize xs tokens (p2 + 1) 0
                        space           = tokenize xs tokens p1 (p2 + 1)

        tokenizeKeyword :: String -> Tokens -> Int -> Int -> String -> Tokens
        tokenizeKeyword [] tokens p1 p2 result
                | isKeyword = tokens ++ [Keyword result p1 p2]
                | otherwise = tokens ++ [Identifier result p1 p2]
                where isKeyword = boolMatchRegex (matchRegex keyword result)
        tokenizeKeyword (x:xs) tokens p1 p2 result
                | isAlphaNum x || x == '_' = tokenizeKeyword xs tokens p1
                        (p2 + 1) (result ++ [x])
                | otherwise = case isKeyword of
                                True    -> tokenize (x:xs)
                                        (tokens ++ [Keyword result p1 p2]) p1 p2
                                False   -> tokenize (x:xs)
                                        (tokens ++ [Identifier result p1 p2]) p1
                                                p2

                where
                        isKeyword       = boolMatchRegex
                                (matchRegex keyword result)

        tokenizeStringLiteral :: String -> Tokens -> Int -> Int -> Bool -> Bool
                -> String -> Tokens
        tokenizeStringLiteral [] tokens p1 p2 True False result = tokens ++
                [StringLiteral result p1 p2]
        tokenizeStringLiteral [] _ p1 p2 False False _ =
                let message = "Quote wasn't closed at " ++ getPosition p1 p2 ++
                                "."
                        in error message

        tokenizeStringLiteral [] _ p1 p2 False True  _ =
                let message = "Char wasn't escaped at " ++ getPosition p1 p2 ++
                                "."
                        in error message

        tokenizeStringLiteral _ _ _ _ True True _ =
                error "String can't be escaped and closed at the same time!"

        tokenizeStringLiteral (_:xs) tokens p1 p2 True False result = tokenize
                xs (tokens ++ [StringLiteral result p1 p2]) p1 p2

        tokenizeStringLiteral (x:xs) tokens p1 p2 False False result
                | x == '\n'     = tokenizeStringLiteral xs tokens (p1 + 1) 0
                        False False result
                | x == '\\'     = tokenizeStringLiteral xs tokens p1 p2 False
                        True addChar
                | x == '\"'     = tokenizeStringLiteral xs tokens p1 p2 True
                        False result
                | otherwise     = tokenizeStringLiteral xs tokens p1 (p2 + 1)
                        False False addChar
                where addChar         = result ++ [x]

        tokenizeStringLiteral (x:xs) tokens p1 p2 False True result =
                let y = case x of
                        't' -> '\t'
                        'b' -> '\b'
                        'n' -> '\n'
                        'r' -> '\r'
                        'f' -> '\f'
                        '\'' -> x
                        '\"' -> x
                        '\\' -> x
                        _    -> case isDigit x of
                                True -> third (nextInteger (x:xs) tokens p1 p2 result)
                        -- Dodam tokenizację kodu unicode lub ascii

        nextInteger :: String -> Int -> String -> (String, Int, String)
        nextInteger [] p result = ([], tokens, p1, p2, result)
        nextInteger (x:xs) p result
                | isDigit x = nextInteger xs  p (result ++ [x])
                | otherwise = ((x:xs), p, result)
