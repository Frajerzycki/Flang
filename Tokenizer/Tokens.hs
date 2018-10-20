module Tokenizer.Tokens (Token (..), Tokens, getLexeme) where
        data Token = IfKeyword {charNumber :: Int, lineNumber :: Int}
                | ElseKeyword {charNumber :: Int, lineNumber :: Int}
                | TypeKeyword {charNumber :: Int, lineNumber :: Int}
                | Operator {value :: String, charNumber :: Int,
                        lineNumber :: Int}
                | Identifier {value :: String, charNumber :: Int,
                        lineNumber :: Int}
                | Semicolon {charNumber :: Int, lineNumber :: Int}
                | Bracket {value :: String, charNumber :: Int, lineNumber :: Int}
                | SquareBracket {value :: String, charNumber :: Int, lineNumber :: Int}
                | Tab {charNumber :: Int, lineNumber :: Int}
                | IntegerLiteral {value :: String, charNumber :: Int,
                        lineNumber:: Int}
                | FloatingLiteral {value :: String, charNumber :: Int,
                        lineNumber:: Int}
                | StringLiteral {value :: String, charNumber :: Int,
                        lineNumber:: Int}
                | CharLiteral {value :: String, charNumber :: Int,
                        lineNumber:: Int} deriving (Show, Eq)
        type Tokens = [Token]

        getLexeme :: String -> Int -> Int -> Token
        getLexeme s p1 p2 = case s of
                "if"            -> IfKeyword p1 p2
                "else"          -> ElseKeyword p1 p2
                "type"          -> TypeKeyword p1 p2
                _               -> Identifier s p1 p2
