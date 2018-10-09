module Tokenizer (Token) where
        import Data.Char
        import Text.Regex
        -- Tokenizing
        data Token = Keyword String Int Int
                | Operator String Int Int
                | Identifier String Int Int
                | RangeOperator Char Int Int
                | IntegerLiteral String Int Int
                | StringLiteral String Int Int
                | CharLiteral Char Int Int
                | BooleanLiteral String Int Int

        type Tokens = [Token]
        keyword = mkRegex "^(if|else|data|int|integer|boolean|string|char)$"
        tokenize :: String -> Tokens -> Int -> Int -> Tokens
        tokenize [] tokens _ _ = tokens
        tokenize (x:xs) tokens position1 position2
                | x == '\t'  || x == ':'        = rangeOperator
                | x == '\n'                     = newLine
                | x == ' '                      = space
                | isAlpha x || x == '_'         = tokenizeKeyword xs tokens
                        position1 withChar [x]
                where
                        rangeOperator   = tokenize xs
                                (tokens ++ [RangeOperator x position1 withLine])
                                position1 (position2 + 1)
                        newLine         = tokenize xs tokens (position1 + 1) 0
                        space           = tokenize xs tokens position1 withChar
                        withChar = (position2 + 1)
                        withLine = (position1 + 1)
        tokenizeKeyword :: String -> Tokens -> Int -> Int -> String -> Tokens
        tokenizeKeyword [] tokens position1 position2 result
                | boolMatchRegex (matchRegex keyword result) = tokenize []
                        (tokens ++ [Keyword result position1 position2 ])
                                position1 position2
                | otherwise = tokenize []
                        (tokens ++ [Identifier result position1 position2 ])
                        position1 position2
        tokenizeKeyword (x:xs) tokens position1 position2 result
                | isAlphaNum x || x == '_' = tokenizeKeyword xs tokens
                        position1 withChar
                        (result ++ [x])
                | otherwise = case isKeyword of
                                True    -> tokenize (x:xs)
                                        (tokens ++ [Keyword result position1
                                                position2])
                                                position1 position2
                                False   -> tokenize (x:xs)
                                        (tokens ++ [Identifier result position1
                                                position2])
                                                position1 position2

                where
                        withChar        = position2 + 1
                        isKeyword       = boolMatchRegex
                                (matchRegex keyword result)


        boolMatchRegex :: Maybe [String] -> Bool
        boolMatchRegex (Just _) = True
        boolMatchRegex Nothing = False
