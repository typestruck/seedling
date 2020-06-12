{-# Language RecordWildCards #-}

module Classifier where

-- this assumes data follows a normal distribution!

import qualified Data.Graph.UGraph             as DGU
import           Data.Graph.UGraph              ( UGraph(..) )
import           Data.Graph.Types               ( Edge(..) )
import qualified Data.Map                      as DM
import           Database                       ( User(..)
                                                , Chat(..)
                                                )
import           Data.Map                       ( (!)
                                                , Map
                                                )

data Feature =  UserPositiveEdges |
                ComparedUserPositiveEdges |
                UserNegativeEdges |
                ComparedUserNegativeEdges |
                -- CommonNeighbors | -- this should be category data too, like no in common, few in common, many etc
                UserTotalDegre |
                ComparedUserTotalDegree |
                -- UserPageRank Int | -- this is category data!
                -- ComparedUserPageRank Int -- this is category data!
                UserAge |
                -- AgeDifference |
                UserAccountAge |
                ComparedUserAccountAge |
                UserKarma
                -- CosineSimilarityDescriptions | -- this is binary data!
                -- CosineSimilarityTags Double -- this is binary data!
                deriving (Eq, Ord, Show, Enum)

data Summary = Summary { feature :: Feature, mean :: Double, standardDeviation :: Double } deriving Show

featurize :: UGraph String Double -> [User] -> [[(Feature, Double)]]
featurize network users = DM.elems
        $ foldl build DM.empty users
    where
        build featureMap user =
                let userName      = name user
                    edges         = DGU.incidentEdges network userName
                    edgeCount     = length edges
                    positiveEdges = length $ filter isPositiveEdge edges
                in  foldl
                            (\fm fd@(f, d) -> DM.insertWith (\_ old -> fd : old) f [fd] fm)
                            featureMap
                            [ (UserPositiveEdges, fromIntegral positiveEdges)
                            , ( UserNegativeEdges
                              , fromIntegral $ edgeCount - positiveEdges
                              )
                            , (UserTotalDegre, fromIntegral edgeCount)
                            , (UserAge       , fromIntegral $ age user)
                            , (UserAccountAge, fromIntegral $ accountAge user)
                            , (UserKarma     , fromIntegral $ totalKarma user)
                            ]

        isPositiveEdge (Edge _ _ v) = v > -1

        usersMap = DM.fromList $ zipWith (\u t -> (name u, t)) users users

summarize :: Double -> [[(Feature, Double)]] -> [Summary]
summarize count = fmap summit
    where
        summit features =
                let values = fmap snd features
                    mean   = sum values / count
                in  Summary
                            { feature           = fst $ head features
                            , mean              = mean
                            , standardDeviation =
                                    sqrt
                                    $ sum [ (v - mean) ** 2.0 | v <- values ]
                                    / (count - 1)
                            }

score :: Double -> [Summary] -> [(Feature, Double)] -> Double
score count summaries features = foldl calculate 1.0 summaries
    where
        featureMap = DM.fromList features

        calculate total Summary {..} = total * calculateProbability
                mean
                standardDeviation
                (featureMap ! feature)

        calculateProbability mean standardDeviation value =
                (1.0 / (sqrt (2.0 * pi) * standardDeviation))
                        * exp
                                  (-(  (value - mean)
                                    ** 2.0
                                    /  (2.0 * standardDeviation ** 2.0)
                                    )
                                  )

t = 5.0

test1 =
        [ [ (UserPositiveEdges, 3.393533211)
          , (UserPositiveEdges, 3.110073483)
          , (UserPositiveEdges, 1.343808831)
          , (UserPositiveEdges, 3.582294042)
          , (UserPositiveEdges, 2.280362439)
          ]
        , [ (UserNegativeEdges, 2.331273381)
          , (UserNegativeEdges, 1.781539638)
          , (UserNegativeEdges, 3.368360954)
          , (UserNegativeEdges, 4.67917911)
          , (UserNegativeEdges, 2.866990263)
          ]
        ]

f1 = [(UserPositiveEdges, 3.393533211), (UserNegativeEdges, 2.331273381)]

test2 =
        [ [ (UserPositiveEdges, 7.423436942)
          , (UserPositiveEdges, 5.745051997)
          , (UserPositiveEdges, 9.172168622)
          , (UserPositiveEdges, 7.792783481)
          , (UserPositiveEdges, 7.939820817)
          ]
        , [ (UserNegativeEdges, 4.696522875)
          , (UserNegativeEdges, 3.533989803)
          , (UserNegativeEdges, 2.511101045)
          , (UserNegativeEdges, 3.424088941)
          , (UserNegativeEdges, 0.791637231)
          ]
        ]
