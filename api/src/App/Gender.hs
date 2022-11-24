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

import App.RNG.Rand  (randomEnum, randomEnumR)
import App.Utils     (applyWhen)
import Data.Char     (toUpper)
import Data.Text     (dropAround, toTitle, unsnoc)
import System.Random (Random (..))

----------------------------------------------------------------------------------------------------

data Gender = Male | Female | Other deriving (Bounded, Enum, Eq)

instance Random Gender where
  randomR = randomEnumR
  random  = randomEnum

newtype GenderNeutral
  = GN [El]
  deriving (Eq, Monoid, Semigroup, Show)

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
  isSentenceStart         = (Just '.' ≡) . fmap snd . unsnoc . dropAround (≡ ' ')
  maybeCapitalized acc fn = applyWhen (isSentenceStart acc) toTitle (fn g)
  renderEl _   (Txt t)         = t
  renderEl _   (PresentVerb v) = v ⊕ if g ≡ Other then "" else "s "
  renderEl acc (Token tok)     = maybeCapitalized acc $ renderToken tok
  renderToken They       = unGn "he" "she" "they"
  renderToken Them       = unGn "his" "her" "their"
  renderToken Theirs     = unGn "his" "hers" "theirs"
  renderToken Their      = unGn "him" "her" "them"
  renderToken Are        = unGn "is" "is" "are"
  renderToken Were       = unGn "was" "was" "were"
  renderToken Themselves = unGn "himself" "herself" "themselves"

uncapitalize ∷ GenderNeutral → GenderNeutral
uncapitalize (GN ((Txt t):xs)) = GN (Txt (uncapTxt t):xs) where
  uncapTxt = toText . uncapStr . toString
  uncapStr []     = []
  uncapStr (c:cs) = toUpper c : cs
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
