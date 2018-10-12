module Tokenizer.Token (Token (..), Tokens) where
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
        
