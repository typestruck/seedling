module Lib
        ( module Database
        , module Network
        , module Classifier
        , createSuggestions
        )
where

import           Database
import           Classifier
import           Network
import qualified Data.Graph.UGraph             as DGU
import qualified Data.Graph.Types              as DGT
import           Data.Graph.UGraph              ( UGraph(..) )
import           Data.Monoid                    ( Sum(..) )


createSuggestions :: [User] -> UGraph String Double -> String -> IO [String]
createSuggestions users network for = do
        let newUsers = []
        putStrLn for
        putStrLn "\n"
        print $ DGU.incidentEdges network for
        pure [""]
