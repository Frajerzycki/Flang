        module Parser.Parser (AST(..), RPN(..), parse) where
        import Tokenizer.Tokens
        import Helpers.ErrorLogging
        import Parser.ParserHelpers
        import Helpers.ErrorLogging
        import Data.Char
        import qualified Data.Map as Map
        data AST = FunctionCall {name :: String, args :: [AST], pos ::  Int}
                | IfStatement {cond :: AST, body :: AST, pos :: Int}
                | Operation {left :: AST, op :: String, right :: AST}
                | Operand {val :: String, pos :: Int} deriving (Show, Eq)

        data RPN = RPNRightBracket {line :: Int}
                | RPNOperator {rpnValue :: String, line :: Int}
                | RPNOperand {rpnValue :: String, line :: Int} deriving (Show, Eq)

        isRPNRightBracket (RPNRightBracket _) = True
        isRPNRightBracket _ = False

        parse :: Tokens -> [RPN] -> [RPN] -> [RPN]
        parse [] os oq = if [] /= os
                then
                        if isRPNRightBracket los
                                then logError
                                        "There isn't bracket to close at " p1 0
                        else if rpnValue los == "("
                                then logError
                                        "Unclosed bracket at  " p1 0
                        else oqso
                else oqso

                where los = last os
                      oqso = oq ++ reverse os
                      p1 = line los

        parse ((Operator s p1 _):xs) [] oq = parse xs [(RPNOperator s p1)] oq
        parse ((RightBracket p1 p2):xs) [] oq = logError
                "There isn't bracket to close at " p1 p2
        parse (x:xs) os oq
                | isNumber x            = parse xs os (oq ++ [RPNOperand (value x) (lineNumber x)])
                | isFunction x          = parse xs (os ++ [RPNOperator (value x) (lineNumber x)]) oq
                | isOperator x          = if hlos /= '('
                        && ((isAlpha hlos || hlos == '_')
                        || plos < pvalX
                        || (plos == pvalX
                        && isLeftAssociative))
                        then parse (x:xs) ios (oq ++ [los])
                        else parse xs (os ++ [RPNOperator (value x) (lineNumber x)]) oq
                | isLeftBracket x       = parse xs (os ++ [RPNOperator "(" (lineNumber x)]) oq
                | isRightBracket x      = if hlos /= '('
                        then parse (x:xs) ios (oq ++ [los]) else parse xs ios oq
                where
                los                                     = last os
                ios                                     = init os
                hlos                                    = head $ rpnValue los
                valX                                    = value x
                plos                                    = lookup (rpnValue los) precedence
                pvalX                                   = lookup valX precedence
                isNumber (FloatingLiteral _ _ _)        = True
                isNumber (IntegerLiteral _ _ _)         = True
                isNumber _                              = False
                isFunction (Identifier _ _ _)           = True
                isFunction _                            = False
                isOperator (Operator _ _ _)             = True
                isOperator _                            = False
                isLeftBracket (LeftBracket _ _)         = True
                isLeftBracket _                         = False
                isRightBracket (RightBracket _ _)       = True
                isRightBracket _                        = False
                isLeftAssociative                       = not $ [hlos] `elem`
                        ["=", "!"]
