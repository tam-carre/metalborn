module App.Character.GenderNeutral
  ( GenderNeutral
  , applyGender
  , pverb
  , their
  , theirs
  , them
  , they
  , txt
  ) where

import App.Character.Gender (Gender (..))
import App.Utils            (applyWhen)
import Data.Text            (dropAround, toTitle, unsnoc)

----------------------------------------------------------------------------------------------------

newtype GenderNeutral
  = GN [El]
  deriving (Eq, Monoid, Semigroup, Show)

instance IsString GenderNeutral where fromString s = GN [Txt . toText $ s]

they, them, their, theirs ∷ GenderNeutral
they    = GN [They]
them    = GN [Them]
their   = GN [Their]
theirs  = GN [Theirs]

-- | A verb in the present tense
pverb ∷ Text → GenderNeutral
pverb v = GN [PresentVerb v]

txt ∷ Text → GenderNeutral
txt t = GN [Txt t]

applyGender ∷ Gender → GenderNeutral → Text
applyGender g (GN els) = loop "" els where
  loop acc []     = acc
  loop acc (x:xs) = loop (acc ⊕ renderEl acc x) xs
  isSentenceStart              = (Just '.' ≡) . fmap snd . unsnoc
  pronoun acc fn               = " " ⊕ applyWhen (isSentenceStart acc) toTitle (fn g) ⊕ " "
  renderEl _   (Txt t)         = dropAround (≡ ' ') t
  renderEl _   (PresentVerb v) = " " ⊕ v ⊕ if g ≡ Other then " " else "s "
  renderEl acc They            = pronoun acc heSheThey
  renderEl acc Them            = pronoun acc himHerThem
  renderEl acc Theirs          = pronoun acc hisHersTheirs
  renderEl acc Their           = pronoun acc hisHerTheir

data El
  = Txt Text
  | PresentVerb Text
  | They
  | Them
  | Their
  | Theirs
  deriving (Eq, Show)

heSheThey ∷ Gender → Text
heSheThey Male   = "he"
heSheThey Female = "she"
heSheThey Other  = "they"

hisHerTheir ∷ Gender → Text
hisHerTheir Male   = "his"
hisHerTheir Female = "her"
hisHerTheir Other  = "their"

hisHersTheirs ∷ Gender → Text
hisHersTheirs Male   = "his"
hisHersTheirs Female = "hers"
hisHersTheirs Other  = "theirs"

himHerThem ∷ Gender → Text
himHerThem Male   = "him"
himHerThem Female = "her"
himHerThem Other  = "them"
