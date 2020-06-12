{-# Language RecordWildCards #-}

module Network where

import           Database                       ( Chat(..) )
import qualified Data.List                     as DL
import qualified Data.Graph.UGraph as DGU
import Data.Graph.UGraph (UGraph(..))
import Data.Graph.Types (Edge(..))

-- | A network is a graph where the edges have signs (-1 to +1)
createNetwork :: [Chat] -> UGraph String Double
createNetwork chats = DGU.fromEdgesList $ map makeEdge chats
        where
                makeEdge Chat {..} =
                    Edge
                    firstUserName
                    secondUserName
                    $ if blocked then -1.0 else normalize $ fromIntegral karma

                normalize k = (k - minimumKarma) / (maximumKarma - minimumKarma)

                karmas       = filter ( > 0 ) $ map (\Chat {..} -> fromIntegral karma) chats
                minimumKarma = minimum karmas
                maximumKarma = maximum karmas

