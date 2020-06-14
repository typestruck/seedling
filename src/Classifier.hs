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
import qualified Data.List                     as DL
import           Data.Map                       ( (!)
                                                , Map
                                                )
import           Data.Maybe                    as DMY

data Summary = Summary { feature :: Feature, mean :: Double, standardDeviation :: Double } deriving Show

data Classification = Bad | Neutral | Good deriving (Show, Eq, Ord,Enum, Bounded)

data Feature =  UserPositiveEdges |
                ComparedUserPositiveEdges |
                UserNegativeEdges |
                ComparedUserNegativeEdges |
                CommonNeighbors | -- maybe this should be category data too, like no in common, few in common, many etc
                UserTotalDegree |
                ComparedUserTotalDegree |
                -- UserPageRank Int | -- this is category data!
                -- ComparedUserPageRank Int -- this is category data!
                UserAge |
                AgeDifference |
                UserAccountAge |
                ComparedUserAccountAge |
                UserKarma
                -- CosineSimilarityDescriptions | -- this is binary data!
                -- CosineSimilarityTags Double -- this is binary data!
                deriving (Eq, Ord, Show, Enum)

featurize
        :: UGraph String Double
        -> [Chat]
        -> [User]
        -> [[(Feature, Double, Classification)]]
featurize network chats users = DM.elems $ foldl build DM.empty chats
    where
        build featureMap Chat {..} =
                let
                        (user, comparedUser) =
                                ( usersMap ! secondUserName
                                , usersMap ! secondUserName
                                )
                        (userEdges, comparedUserEdges) =
                                ( DGU.incidentEdges network $ name user
                                , DGU.incidentEdges network $ name comparedUser
                                )
                        (userEdgeCount, comparedUserEdgeCount) =
                                (length userEdges, length comparedUserEdges)
                        (userPositiveEdges, comparedUserPositiveEdges) =
                                ( length $ filter isPositiveEdge userEdges
                                , length $ filter isPositiveEdge
                                                  comparedUserEdges
                                )
                        classification = classify . DMY.fromJust $ DL.find
                                (\(Edge _ b v) -> b == name comparedUser)
                                userEdges
                in
                        foldl
                                insertFeature
                                featureMap
                                [ ( UserPositiveEdges
                                  , fromIntegral userPositiveEdges
                                  , classification
                                  )
                                , ( ComparedUserPositiveEdges
                                  , fromIntegral comparedUserPositiveEdges
                                  , classification
                                  )
                                , ( UserNegativeEdges
                                  , fromIntegral
                                  $ userEdgeCount
                                  - userPositiveEdges
                                  , classification
                                  )
                                , ( ComparedUserNegativeEdges
                                  , fromIntegral
                                  $ comparedUserEdgeCount
                                  - comparedUserPositiveEdges
                                  , classification
                                  )
                                , ( UserTotalDegree
                                  , fromIntegral userEdgeCount
                                  , classification
                                  )
                                , ( ComparedUserTotalDegree
                                  , fromIntegral comparedUserEdgeCount
                                  , classification
                                  )
                                , ( UserAge
                                  , fromIntegral $ age user
                                  , classification
                                  )
                                , ( AgeDifference
                                  , fromIntegral
                                  . abs
                                  $ age comparedUser
                                  - age comparedUser
                                  , classification
                                  )
                                , ( CommonNeighbors
                                  , fromIntegral . length $ DL.intersect
                                          userEdges
                                          comparedUserEdges
                                  , classification
                                  )
                                , ( UserAccountAge
                                  , fromIntegral $ accountAge user
                                  , classification
                                  )
                                , ( ComparedUserAccountAge
                                  , fromIntegral $ accountAge comparedUser
                                  , classification
                                  )
                                , ( UserKarma
                                  , fromIntegral $ totalKarma user
                                  , classification
                                  )
                                ]
        insertFeature featureMap fdc@(feature, _, _) =
                DM.insertWith (\_ old -> fdc : old) feature [fdc] featureMap

        usersMap       = DM.fromList $ zipWith (\u t -> (name u, t)) users users

        isPositiveEdge = (== Good) . classify

        classify (Edge _ _ v) | v >= 0.5  = Good
                              | v < 0.5   = Neutral
                              | otherwise = Bad

summarize :: [[(Feature, Double, Classification)]]        -> [(Classification, Double, [Summary])]
summarize allFeatures =
        [ (classification, fromIntegral . length $ head filtered, fmap summit filtered)
        | classification <- [minBound ..],
          let filtered = filterClass classification allFeatures
        ]
    where
        filterClass classification =
                fmap (fmap extract . filter (sameClass classification))
        extract (feature, weight, _) = (feature, weight)
        sameClass classification (_, _, c) = c == classification
        summit features =
                let count = fromIntegral $ length features
                    values = fmap snd features
                    mean   = sum values / count
                in  Summary
                            {
                              feature           = fst $ head features
                            , mean              = mean
                            , standardDeviation =
                                    sqrt
                                    $ sum [ (v - mean) ** 2.0 | v <- values ]
                                    / (count - 1)
                            }

--adapt to using classes

score :: Double -> [(Classification, Double, [Summary])] -> [(Feature, Double)] -> [(Classification, Double)]
score totalCount summaries features = fmap foldScore summaries
    where
        foldScore (classification, count, summaries) = (classification, foldl calculate (count / totalCount) summaries)

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
