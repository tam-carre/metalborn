module App.Metal (Metal (..)) where

----------------------------------------------------------------------------------------------------

data Metal = Iron | Steel | Tin | Pewter | Zinc | Brass | Copper | Bronze | Cadmium | Bendalloy | Gold | Electrum | Chromium | Nicrosil | Aluminum | Duralumin deriving
  ( Bounded
  , Enum
  , Eq
  , Show
  )
