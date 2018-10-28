module Tokenizer.Tokenizer (tokenize) where
        import Data.Char
        import Tokenizer.TokenizerHelpers
        import Helpers.ErrorLogging
        import Tokenizer.Tokens
        -- Tokenizing

        -- Tokenize Flang
        tokenize :: String -> Tokens -> Int -> Int -> Tokens
        tokenize [] tokens _ _ = tokens
        tokenize (x:xs) tokens p1 p2
                | x `elem`['\t', ';', '[', ']', '(', ')']
                                                = oneChar
                | x == '\n'                     = newLine
                | x == ' '                      = space
                | isAlpha x || x == '_'         = keywordOrIdentifier
                | isDigit x || isNumberWithChar = number
                | isShiftOperator               = operator [x]
                | isComparisonOperator          = operator "="
                | x `elem` operators            = operator []
                | x == '\"'                     = stringLiteral
                | x == '\''                     = charLiteral
                | otherwise                     = logError
                        "Not used char in Flang at " p1 p2
                where
                        tokenOneChar          = case x of
                                '\t'    -> [Tab p1 withChar]
                                ','     -> [Comma p1 withChar]
                                ';'     -> [Semicolon p1 withChar]
                                '('     -> [LeftBracket "(" p1 withChar]
                                ')'     -> [RightBracket p1 withChar]
                                _       -> [SquareBracket [x] p1 withChar]

                        oneChar               = tokenize xs
                                (tokens ++ tokenOneChar) p1 withChar
                        newLine                 = tokenize xs tokens (p1 + 1) 1
                        space                   = tokenize xs tokens p1 withChar
                        keywordOrIdentifier     = tokenizeKeyword xs tokens
                                p1 withChar [x]
                        stringLiteral           = tokenizeStringLiteral xs
                                tokens p1 withChar False False []
                        charLiteral             = tokenizeCharLiteral xs tokens
                                p1 withChar
                        number                  = tokenizeNumberLiteral rest
                                tokens p1 p2 (char isNumberWithChar)
                        operator y       =
                                let
                                        lenY      = length y
                                        withChars = withChar + lenY

                                in tokenize (drop lenY xs) (tokens ++
                                        [Operator ([x] ++ y) p1 withChars]) p1
                                        withChars

                        withChar                = (p2 + 1)
                        isNumberLiteral (IntegerLiteral _ _ _)
                                                = True
                        isNumberLiteral (FloatingLiteral _ _ _)
                                                = True
                        isNumberLiteral _       = False

                        isNumberWithChar = x `elem` ['+', '-'] && ([] /= xs) &&
                                isDigit (head xs)  && (([] /= tokens &&
                                not (isNumberLiteral $ last tokens)) ||
                                (null tokens))

                        char b                   = case b of
                                                        True    -> [x]
                                                        False   -> []

                        rest                    = char (not isNumberWithChar)
                                ++ xs

                        operators               =
                                ['+', '-', '*', '/', '&', '|',
                                        '^', '!', '<', '>', '=']
                        isTwoCharOperator y     = [] /= xs && head xs == y
                        isShiftOperator         = let
                                        txs             = tail xs
                                        tx              = head txs
                                        in isTwoCharOperator x && x
                                        `elem` ['<', '>'] && (([] /=
                                        txs && tx /= '=') || null txs)
                        isComparisonOperator    = isTwoCharOperator '=' && x
                                `elem` ['<','>', '!', '=']

        -- Tokenize keyword
        tokenizeKeyword :: String -> Tokens -> Int -> Int -> String -> Tokens

        tokenizeKeyword [] tokens p1 p2 result = tokens ++
                [(getLexeme result p1 p2)]

        tokenizeKeyword (x:xs) tokens p1 p2 result
                | isAlphaNum x || x == '_'      = tokenizeKeyword xs tokens p1
                        (p2 + 1) (result ++ [x])
                | otherwise                     = tokenize (x:xs)
                        (tokens ++ [(getLexeme result p1 p2)]) p1 p2

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
                        unescapedChar   = unescapeChar (x:xs) p1 p2
                        y               = first unescapedChar
                        in tokenizeStringLiteral (second unescapedChar) tokens
                                p1 (third unescapedChar) False False
                                (result ++ [y])

        -- Tokenize Number Literal
        tokenizeNumberLiteral :: String -> Tokens -> Int -> Int -> String
                -> Tokens

        tokenizeNumberLiteral xs tokens p1 p2 c
                | isFloating = tokenize rAfterFloating (tokens ++
                        [FloatingLiteral (c ++ floating) p1 pAfterFloating]) p1
                                pAfterFloating
                | otherwise = tokenize rAfterInteger (tokens ++ [IntegerLiteral
                        (c ++ integer) p1 pAfterInteger]) p1 pAfterInteger
                where
                        nextInt         = nextInteger xs p2 []
                        tailXs          = tail rAfterInteger
                        headXs          = head rAfterInteger
                        afterDot        = nextInteger tailXs (second nextInt) []
                        rAfterFloating  = first afterDot
                        rAfterInteger   = first nextInt
                        pAfterFloating  = second afterDot
                        pAfterInteger   = second nextInt
                        floatingPoint   = third afterDot
                        integer         = third nextInt
                        floating        = (integer ++ "." ++ floatingPoint)
                        isFloating      = rAfterInteger /= [] && tailXs /= [] &&
                                (isDigit $ head tailXs) && (headXs == '.')

        tokenizeCharLiteral :: String -> Tokens -> Int -> Int -> Tokens
        tokenizeCharLiteral [] _ p1 p2 = logError "There isn't any char at " p1
                p2
        tokenizeCharLiteral (x:xs) tokens p1 p2
                | x == '\\' = tokenize (doesSingleQuoteClosed (second
                        unescapedChar) p1 position) (tokens ++ [CharLiteral
                        [first unescapedChar] p1 (position + 1)]) p1 (position
                        + 1)
                | otherwise = tokenize (doesSingleQuoteClosed xs p1 (p2 + 1))
                        (tokens ++ [CharLiteral [x] p1 (p2 + 2)]) p1 (p2 + 1)
                where
                        unescapedChar           = unescapeChar xs p1 (p2 + 1)
                        position                = third unescapedChar

        doesSingleQuoteClosed :: String -> Int -> Int -> String
        doesSingleQuoteClosed [] p1 p2 = logError "Quote wasn't closed at " p1
                p2
        doesSingleQuoteClosed (x:xs) p1 p2 = case x of
                '\''    -> xs
                _       -> logError "Quote wasn't closed at " p1 p2
