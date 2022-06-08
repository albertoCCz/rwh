-- default
-- main = interact wordCount
--     where wordCount input = "# lines: " ++ show (length (lines input)) ++ "\n"

-- count words in file
main = interact wordCount
    where wordCount input = "# words: " ++ show (length (words input)) ++ "\n"

-- count characters in file
-- main = interact wordCount
--     where wordCount input = "# chars: " ++ show (length input) ++ "\n"
