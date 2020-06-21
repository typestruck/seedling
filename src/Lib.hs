module Lib
        ( createSuggestions
        )
where

import           Database                       ( Chat(..)
                                                , User(..)
                                                )
import qualified Classifier                    as C
import           Classifier                     ( Classification(..) )
import qualified Network                       as N
import qualified Data.List                     as DL
import           Data.HashMap.Lazy              ( (!)
                                                , HashMap
                                                )
import qualified Data.HashMap.Lazy             as DM
import qualified Data.Graph.UGraph             as DGU
import           Debug.Trace

type Bucket = Int

createSuggestions
        :: [Chat]
        -> [User]
        -> User
        -> HashMap Classification [(String, Double, Bucket)]
createSuggestions chats users for = sortValues createMap
    where
        sortValues = DM.map addBucket . DM.map (DL.sortBy compareScore)
        addBucket values =
                let total = max 1 $ length values
                in  zipWith
                            (\(useName, scoreNormal) i ->
                                    (useName, scoreNormal, i `div` (max 5 total `div` 5))
                            )
                            values
                            [1 .. total]

        createMap = foldl scoreAndInsert DM.empty users
        scoreAndInsert classMap comparedUser =
                let
                        comparedUserEdges = getEdges comparedUser
                        s = C.scoreNormal chatsCount summaries

                                        $  userFeatures
                                        ++ C.featurizeComparedUser
                                                   comparedUserEdges
                                                   comparedUser
                                        ++ C.featurizeCommon
                                                   userEdges
                                                   for
                                                   comparedUserEdges
                                                   comparedUser
                        (classification, scoreNormal) =
                                DL.maximumBy compareScore
                                        $ trace ("scores" ++ show s) s
                        new = (name comparedUser, scoreNormal)
                in
                        DM.insertWith (\_ old -> new : old)
                                      classification
                                      [new]
                                      classMap

        compareScore s s2 = compare (snd s) (snd s2)
        getEdges     = DGU.incidentEdges network . name

        userEdges    = getEdges for
        userFeatures = C.featurizeUser userEdges for

        chatsCount   = fromIntegral $ length chats

        network      = N.createNetwork chats
        features     = C.featurize network chats users
        summaries    = trace ("summaries " ++ show (C.summarize features))(C.summarize features)
