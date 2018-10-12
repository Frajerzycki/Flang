module Tokenizer.ErrorLogging (logError, getPosition) where
        getPosition :: Int -> Int -> String
        getPosition p1 p2 = (show p1) ++ ":" ++ (show p2)

        logError :: String -> Int -> Int -> a
        logError  str p1 p2 =
                let message = str ++ getPosition p1 p2 ++ "." in error message
