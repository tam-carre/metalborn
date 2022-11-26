{-# LANGUAGE OverloadedLists, TemplateHaskell #-}

module App.Gender
  ( Gender (..)
  , GenderNeutral
  , applyGender
  , are
  , pverb
  , their
  , theirs
  , them
  , themselves
  , they
  , txt
  , uncapitalize
  , were
  ) where

import App.RNG.Rand  (randomEl, randomEnumR)
import Data.Char     (toLower)
import Data.Text     qualified as T
import Servant.Elm   (defaultOptions, deriveBoth)
import System.Random (Random (..))

----------------------------------------------------------------------------------------------------

data Gender = Male | Female | Other deriving (Bounded, Enum, Eq, Generic, Show)

deriveBoth defaultOptions ''Gender

instance Random Gender where
  randomR = randomEnumR
  random  = randomEl [Male, Female]

newtype GenderNeutral
  = GN [El]
  deriving (Eq, Show)

instance Semigroup GenderNeutral where
  (<>) (GN a) (GN b) = GN (a ⊕ b)

instance Monoid GenderNeutral where
  mempty = GN []

instance IsString GenderNeutral where fromString s = GN [Txt . toText $ s]

they, them, their, theirs, are, were, themselves ∷ GenderNeutral
they       = GN [Token They]
them       = GN [Token Them]
their      = GN [Token Their]
theirs     = GN [Token Theirs]
are        = GN [Token Are]
were       = GN [Token Were]
themselves = GN [Token Themselves]

-- | A verb in the present tense
pverb ∷ Text → GenderNeutral
pverb v = GN [PresentVerb v]

txt ∷ Text → GenderNeutral
txt t = GN [Txt t]

applyGender ∷ Gender → GenderNeutral → Text
applyGender g (GN els) = loop "" els where
  loop acc []     = acc
  loop acc (x:xs) = loop (acc ⊕ renderEl acc x) xs
  isSentenceStart         = (Just '.' ≡) . fmap snd . T.unsnoc . T.dropAround (≡ ' ')
  maybeCapitalized acc fn = applyWhen (isSentenceStart acc) T.toTitle (fn g)
  renderEl _   (Txt t)         = t
  renderEl _   (PresentVerb v) = v ⊕ if g ≡ Other then "" else "s "
  renderEl acc (Token tok)     = maybeCapitalized acc $ renderToken tok
  renderToken They       = unGn "he" "she" "they"
  renderToken Them       = unGn "him" "her" "them"
  renderToken Theirs     = unGn "his" "hers" "theirs"
  renderToken Their      = unGn "hi" "her" "their"
  renderToken Are        = unGn "is" "is" "are"
  renderToken Were       = unGn "was" "was" "were"
  renderToken Themselves = unGn "himself" "herself" "themselves"

-- | Bad and hacky
uncapitalize ∷ GenderNeutral → GenderNeutral
uncapitalize (GN ((Txt t):xs)) = GN (Txt (uncapTxt t):xs) where
  uncapTxt = toText . uncapStr . toString
  uncapStr []     = []
  uncapStr (c:cs) = toLower c : cs
uncapitalize other = other

data El
  = Txt Text
  | PresentVerb Text
  | Token Token
  deriving (Eq, Show)

data Token = They | Them | Their | Theirs | Themselves | Are | Were deriving (Eq, Show)

unGn ∷ a → a → a → Gender → a
unGn a _ _ Male   = a
unGn _ a _ Female = a
unGn _ _ a Other  = a
