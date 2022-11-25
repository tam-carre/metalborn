module App.Utils (applyWhen, justIf, pascalToTitleCase, pp) where

import Data.Char (isUpper)

----------------------------------------------------------------------------------------------------

-- | Useful to stringify data constructors
pascalToTitleCase ∷ String → String
pascalToTitleCase ""     = ""
pascalToTitleCase (c:cs) = c:loop cs where
  loop "" = ""
  loop (x:xs) | isUpper x = ' ':x:loop xs
              | otherwise = x:loop xs

pp ∷ Show a ⇒ a → String
pp = pascalToTitleCase . show

applyWhen ∷ Bool → (a → a) → a → a
applyWhen cond f a = if cond then f a else a

justIf ∷ Bool → a → Maybe a
justIf cond a = if cond then Just a else Nothing
