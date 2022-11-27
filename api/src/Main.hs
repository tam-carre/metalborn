module Main (main) where

import App.Docs   (generateDocs)
import App.Elm    (generateElmCode)
import App.Server (runServer)

----------------------------------------------------------------------------------------------------

main ∷ IO ()
main = do
  generateDocs    ≫ echo "Endpoint documentation generated."
  generateElmCode ≫ echo "Elm API types and functions generated."
  runServer 8081  ≪ echo "Starting Metalborn API server."

