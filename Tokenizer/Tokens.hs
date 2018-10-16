module Tokenizer.Tokens (Token (..), Tokens, getLexeme) where
        data Token = IfKeyword Int Int
                | ElseKeyword Int Int
                | TypeKeyword Int Int
                | Operator String Int Int
                | Identifier String Int Int
                | Semicolon Int Int
                | Bracket Int Int
                | SquareBracket Int Int
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
                "type"          -> TypeKeyword p1 p2
                _               -> Identifier s p1 p2
