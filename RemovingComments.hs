module RemovingComments (removeComments)
where
        -- Removing comments
        removeComments :: String -> Bool -> Bool -> String -> String
        removeComments [] _ _ s = s
        removeComments (x:xs) False False s
               | x == '#' = removeComments xs True False s
               | otherwise = removeComments xs False (x == '\"') (s ++ [x])

        removeComments (x:xs) False True s =
                removeComments xs False (not $ x == '\"')(s ++ [x])

        removeComments (x:xs) True False s =
                removeComments xs (not $ x == '\n') False s

        removeComments _ True True _ = error "Comment can't be in string!"
