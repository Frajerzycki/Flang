module Parser (AST(..)) where
        import Tokenizer.Tokens
        import Helpers.ErrorLogging
        data AST = Function {name :: String, args :: [AST], pos ::  Int}
                | IfStatement {cond :: AST, body :: AST, pos :: Int}
                | Operation {left :: AST, op :: String, right :: AST}
                | Operand {val :: String, pos :: Int} deriving Show

        addNode :: [AST] -> String -> [AST]
        addNode ts op = ts ++ [Operation left op right]
                where
                        right   = last ts
                        left    = last $ init ts
        parseExpression :: Tokens -> [AST] -> [String] -> AST
        parseExpression [] ts [] = last ts
        parseExpression [] ts os = parseExpression [] (addNode ts (last os))
                (init os)
        parseExpression ((RightBracket _ _):xs) ts [] = parseExpression xs ts []
        parseExpression (x:xs) ts os
                | isOperand x   = parseExpression xs (ts ++ [Operand (value x)
                        (lineNumber x)]) os
                | isOperator x || isLeftBracket x =
                        parseExpression xs ts (os ++ [value x])
                | isRightBracket x = case o of
                        "("     -> parseExpression xs ts os
                        _       -> parseExpression (x:xs) (addNode ts o)
                                (init os)
                where
                        isLeftBracket  (LeftBracket _ _ _)      = True
                        isLeftBracket  _                        = False
                        isRightBracket  (RightBracket _ _)      = True
                        isRightBracket  _                       = False
                        isOperator (Operator _ _ _)             = True
                        isOperator _                            = False
                        isOperand (IntegerLiteral _ _ _)        = True
                        isOperand (FloatingLiteral _ _ _)       = True
                        isOperand (CharLiteral _ _ _)           = True
                        isOperand (StringLiteral _ _ _)         = True
                        isOperand (Identifier _ _ _)            = True
                        isOperand _                             = False
                        o                                       = last os
