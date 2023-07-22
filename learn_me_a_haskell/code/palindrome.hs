-- file: palindrome.hs
-- author: Jacob Xie
-- date: 2023/07/22 22:14:16 Saturday
-- brief:

main = interact respondPalindromes

-- respondPalindromes contents =
--   unlines
--     ( map
--         ( \xs ->
--             if isPalindrome xs then "palindrome" else "not a palindrome"
--         )
--         (lines contents)
--     )
--   where
--     isPalindrome xs = xs == reverse xs

respondPalindromes =
  unlines
    . map
      (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome")
    . lines
  where
    isPalindrome xs = xs == reverse xs
