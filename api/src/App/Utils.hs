module App.Utils (pascalToTitleCase) where

import Data.Char (isUpper)

----------------------------------------------------------------------------------------------------

-- | Useful to stringify data constructors
pascalToTitleCase ∷ String → String
pascalToTitleCase ""     = ""
pascalToTitleCase (c:cs) = c:loop cs where
  loop "" = ""
  loop (x:xs) | isUpper x = ' ':x:loop cs
              | otherwise = x:loop xs
