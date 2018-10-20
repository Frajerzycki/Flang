module Parser (AST(..)) where
        import Tokenizer.Tokens
        data AST = Function String [AST] AST Int
                | IfStatement AST AST Int
                | Operation AST String AST Int
                | Operand String Int


        parseExpression :: Tokens -> [AST] -> [String] -> AST
        parseExpression [] ts _ = last ts
        parseExpression (x:xs) ts os
                | isOperand x   = parseExpression xs (ts ++ [Operand (value x)
                        (lineNumber x)]) os
                | isFunction x  = parseFunction xs ts os 1 0
                | isOperator x  = parseExpression xs ts ([value x] ++ os)
                where
                        isBracket  (Bracket _ _ _)              = True
                        isBracket  _                            = False
                        isOperator (Operator _ _ _)             = True
                        isOperator _                            = False
                        isOperand (IntegerLiteral _ _ _)        = True
                        isOperand (FloatingLiteral _ _ _)       = True
                        isOperand (CharLiteral _ _ _)           = True
                        isOperand (StringLiteral _ _ _)         = True
                        isOperand (Identifier _ _ _)            = True
                        isOperand _                             = False
                        isFunction (Identifier _ _ _)           = True
                                && not (null xs) && (not . isBracket $ head xs)
                        isFunction _                            = False
