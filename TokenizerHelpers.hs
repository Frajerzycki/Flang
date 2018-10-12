module TokenizerHelpers(getPosition, boolMatchRegex, escape, keyword, third)
        where
        import Text.Regex
        import Text.ParserCombinators.ReadP
        import Text.Read.Lex

        getPosition :: Int -> Int -> String
        getPosition p1 p2 = (show p1) ++ ":" ++ (show p2)

        boolMatchRegex :: Maybe [String] -> Bool
        boolMatchRegex (Just _) = True
        boolMatchRegex Nothing  = False

        escape :: String -> String
        escape xs
                | []      <- r = []
                | [(a,_)] <- r = a
                | otherwise = []
                where r = readP_to_S (manyTill lexChar eof) xs

        keyword :: Regex
        keyword = mkRegex "^(if|else|data|int|integer|boolean|string|char)$"

        third :: (a, c, a) -> a
        third (_, _, s) = s
