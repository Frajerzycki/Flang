module Tokenizer.Token (Token (..), Tokens, getLexeme) where
        data Token = IfKeyword Int Int
                | ElseKeyword Int Int
                | IntKeyword Int Int
                | FloatKeyword Int Int
                | StringKeyword Int Int
                | CharKeyword Int Int
                | StructKeyword Int Int
                | TrueKeyword Int Int
                | FalseKeyword Int Int
                | Operator String Int Int
                | Identifier String Int Int
                | Semicolon Int Int
                | Tab Int Int
                | IntegerLiteral String Int Int
                | FloatingLiteral String Int Int
                | StringLiteral String Int Int
                | CharLiteral Char Int Int deriving (Show, Eq)
        type Tokens = [Token]

        getLexeme :: String -> Int -> Int -> Token
        getLexeme s p1 p2 = case s of
                "if"            -> IfKeyword p1 p2
                "else"          -> ElseKeyword p1 p2
                "int"           -> IntKeyword p1 p2
                "float"         -> FloatKeyword p1 p2
                "string"        -> StringKeyword p1 p2
                "char"          -> CharKeyword p1 p2
                "struct"        -> StructKeyword p1 p2
                "true"          -> TrueKeyword p1 p2
                "false"         -> FalseKeyword p1 p2
                _               -> Identifier s p1 p2
