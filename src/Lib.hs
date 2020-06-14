module Lib
        ( module Database
        , module Network
        , module Classifier
        , createSuggestions
        )
where

import           Database
import           Classifier
import           qualified Classifier as C
import           Network
import qualified Data.Graph.UGraph             as DGU
import qualified Data.Graph.Types              as DGT
import           Data.Graph.UGraph              ( UGraph(..) )
import           Data.Monoid                    ( Sum(..) )

createSuggestions :: Double -> [Summary] -> String -> IO [String]
createSuggestions count summary for = do
        C.score count summary [(Feature, Double)]
        pure [""]