{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Docs (generateDocs) where

import App.Server   (siteAPI)
import Servant.Docs (API, ToSample (..), docs, markdown, singleSample)

----------------------------------------------------------------------------------------------------

instance ToSample Text where toSamples _ = singleSample "Kaladin"

apiDocs ∷ API
apiDocs = docs siteAPI

generateDocs ∷ IO ()
generateDocs =
  writeFile "./docs/Endpoints.md" $ apiDocsIntro ⊕ markdown apiDocs
  where
  apiDocsIntro = toString $ unlines
    [ "*Notes:*"
    , "- This documentation was automatically generated by Servant."
    , "- Except for manual testing, you are not supposed to concern yourself with how the data is serialized; that's all glue code invisible to the developer under normal circumstances. `Elm.hs` generates Elm functions that automatically encode and decode data, and Servant generates Haskell encoders/parsers with TH-generated FromJSON and ToJSON instances.\n"
    ]

