module Tokenizer.Tokenizer (tokenize) where
        import Data.Char
        import Text.Regex
        import Tokenizer.TokenizerHelpers
        import Tokenizer.ErrorLogging
        import Tokenizer.Token
        -- Tokenizing

        -- Tokenize Flang
        tokenize :: String -> Tokens -> Int -> Int -> Tokens
        tokenize [] tokens _ _ = tokens
        tokenize (x:xs) tokens p1 p2
                | x == '\t'  || x == ':'        = rangeOperator
                | x == '\n'                     = newLine
                | x == ' '                      = space
                | isAlpha x || x == '_'         = keywordOrIdentifier
                | isDigit x                     = number
                | x == '\"'                     = stringLiteral
                | x == '\''                     = charLiteral
                | otherwise                     = logError
                        "Not used char in Flang at " p1 p2
                where
                        rangeOperator   = tokenize xs
                                (tokens ++ [RangeOperator x p1 (p2 + 1)]) p1
                                        withChar
                        newLine                 = tokenize xs tokens (p1 + 1) 1
                        space                   = tokenize xs tokens p1 withChar
                        keywordOrIdentifier     = tokenizeKeyword xs tokens
                                p1 withChar [x]
                        stringLiteral           = tokenizeStringLiteral xs
                                tokens p1 withChar False False []
                        charLiteral             = tokenizeCharLiteral xs tokens
                                p1 withChar
                        number                  = tokenizeNumberLiteral (x:xs)
                                tokens p1 p2
                        withChar                = (p2 + 1)


        -- Tokenize keyword
        tokenizeKeyword :: String -> Tokens -> Int -> Int -> String -> Tokens

        tokenizeKeyword [] tokens p1 p2 result
                | isKeyword = tokens ++ [Keyword result p1 p2]
                | otherwise = tokens ++ [Identifier result p1 p2]
                where isKeyword = boolMatchRegex (matchRegex keyword result)

        tokenizeKeyword (x:xs) tokens p1 p2 result
                | isAlphaNum x || x == '_' = tokenizeKeyword xs tokens p1
                        (p2 + 1) (result ++ [x])
                | otherwise             = case isKeyword of
                                True    -> tokenize (x:xs)
                                        (tokens ++ [Keyword result p1 p2]) p1 p2
                                False   -> case isBoolLiteral of
                                        True    -> tokenize (x:xs)
                                                (tokens ++ [BooleanLiteral
                                                result p1 p2]) p1 p2
                                        False   -> tokenize (x:xs) (tokens ++
                                                [Identifier result p1 p2]) p1 p2
                where
                        isKeyword               =
                                boolMatchRegex $ matchRegex keyword result
                        isBoolLiteral        =
                                boolMatchRegex $ matchRegex boolLiteral result

        -- Tokenize String Literal

        tokenizeStringLiteral :: String -> Tokens -> Int -> Int -> Bool -> Bool
                -> String -> Tokens

        tokenizeStringLiteral [] tokens p1 p2 True False result = tokens ++
                [StringLiteral result p1 p2]

        tokenizeStringLiteral [] _ p1 p2 False False _ = logError
                "Quote wasn't closed at " p1 p2

        tokenizeStringLiteral [] _ p1 p2 False True  _ = logError
                "Char wasn't escaped at " p1 p2

        tokenizeStringLiteral _ _ _ _ True True _ =
                error "String can't be escaped and closed at the same time!"

        tokenizeStringLiteral xs tokens p1 p2 True False result = tokenize
                xs (tokens ++ [StringLiteral result p1 p2]) p1 p2

        tokenizeStringLiteral (x:xs) tokens p1 p2 False False result
                | x == '\n'     = tokenizeStringLiteral xs tokens (p1 + 1) 1
                        False False result
                | x == '\\'     = tokenizeStringLiteral xs tokens p1 (p2 + 1)
                        False True result
                | x == '\"'     = tokenizeStringLiteral xs tokens p1 (p2 + 1)
                        True False result
                | otherwise     = tokenizeStringLiteral xs tokens p1 (p2 + 1)
                        False False addChar
                where addChar         = result ++ [x]

        tokenizeStringLiteral (x:xs) tokens p1 p2 False True result =
                let
                        unescapedChar     = unescapeChar (x:xs) p1 p2
                        y               = first unescapedChar
                        in tokenizeStringLiteral (second unescapedChar) tokens p1
                                (third unescapedChar) False False (result ++ [y])

        -- Tokenize Number Literal
        tokenizeNumberLiteral :: String -> Tokens -> Int -> Int -> Tokens

        tokenizeNumberLiteral xs tokens p1 p2
                | isFloating = tokenize rAfterFloating (tokens ++
                        [FloatingLiteral floating p1 pAfterFloating]) p1
                                pAfterFloating
                | otherwise = tokenize rAfterInteger (tokens ++ [IntegerLiteral
                        integer p1 pAfterInteger]) p1 pAfterInteger
                where
                        nextInt         = nextInteger xs p2 []
                        tailXs          = tail rAfterInteger
                        headXs  = head rAfterInteger
                        afterDot        = nextInteger tailXs (second nextInt) []
                        rAfterFloating  = first afterDot
                        rAfterInteger   = first nextInt
                        pAfterFloating  = second afterDot
                        pAfterInteger   = second nextInt
                        floatingPoint   = third afterDot
                        integer         = third nextInt
                        floating        = (integer ++ "." ++ floatingPoint)
                        isFloating      = rAfterInteger /= [] &&
                                (isDigit $ head tailXs) && (headXs == '.')

        tokenizeCharLiteral :: String -> Tokens -> Int -> Int -> Tokens
        tokenizeCharLiteral [] _ p1 p2 = logError "There isn't any char at " p1
                p2
        tokenizeCharLiteral (x:xs) tokens p1 p2
                | x == '\\' = tokenize (doesSingleQuoteClosed (second
                        unescapedChar) p1 position) (tokens ++ [CharLiteral
                        (first unescapedChar) p1 (position + 1)]) p1 (position
                        + 1)

                | otherwise = tokenize (doesSingleQuoteClosed xs p1 (p2 + 1))
                        (tokens ++ [CharLiteral x p1 (p2 + 2)]) p1 (p2 + 1)
                where
                        unescapedChar           = unescapeChar xs p1 (p2 + 1)
                        position                = third unescapedChar

        doesSingleQuoteClosed :: String -> Int -> Int -> String
        doesSingleQuoteClosed [] p1 p2 = logError "Quote wasn't closed at " p1
                p2
        doesSingleQuoteClosed (x:xs) p1 p2 = case x of
                '\''    -> xs
                _       -> logError "Quote wasn't closed at " p1 p2
