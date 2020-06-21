{-# Language RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Classifier where

import qualified Data.Graph.UGraph             as DGU
import           Data.Graph.UGraph              ( UGraph(..) )
import           Data.Graph.Types               ( Edge(..) )
import qualified Data.HashMap.Lazy             as DM
import           Database                       ( User(..)
                                                , Chat(..)
                                                )
import           Debug.Trace
import           Data.HashMap.Lazy              ( (!)
                                                , HashMap
                                                )
import qualified Data.List                     as DL
import           Data.Maybe                    as DMY

import           GHC.Generics                   ( Generic )
import           Data.Hashable
import qualified Statistics.Sample.Normalize   as SSN
import qualified Data.Vector.Unboxed           as DVU
import qualified Data.Maybe                    as DM

data Summary = Summary {
        feature :: Feature,
        mean :: Double,
        standardDeviation :: Double
 } deriving Show

data Classification =
        Terrible |
        Bad |
        Neutral |
        Good |
        Excellent deriving (Show, Eq, Ord,Enum, Bounded, Generic)

instance Hashable Classification

--this needs to be reevaluated to find distrubuition, prolly everything not commented is multinominal
data Feature =
        UserPositiveEdges |
        ComparedUserPositiveEdges |
        UserNegativeEdges |
        ComparedUserNegativeEdges |
        CommonNeighbors |
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
        deriving (Eq, Ord, Show, Enum, Generic)

instance Hashable Feature

type FeatureValue = Double

type Count = Double

type FeatureRow = (Feature, FeatureValue, Classification)

-- (+++) :: Unbox a => Vector a -> Vector a -> Vector a
-- (+++) = (DVU.++)

--this can be improved to not repeat values
featurize
        :: UGraph String Double
        -> [Chat]
        -> [User]
        -> [[FeatureRow]]
featurize network chats users = DM.elems $ foldl build DM.empty chats
    where
        build featureMap Chat {..} =
                let
                        (user, comparedUser) =
                                ( usersMap ! firstUserName
                                , usersMap ! secondUserName
                                )
                        (userEdges, comparedUserEdges) =
                                ( DGU.incidentEdges network $ name user
                                , DGU.incidentEdges network $ name comparedUser
                                )

                        classification = classifyEdge . DMY.fromJust $ DL.find
                                (\(Edge _ b v) -> b == name comparedUser)
                                userEdges
                in
                        foldl insertFeature featureMap
                        .  fmap (addClassification classification)
                        $  featurizeCommon userEdges
                                           user
                                           comparedUserEdges
                                           comparedUser
                        ++ featurizeUser userEdges user
                        ++ featurizeComparedUser comparedUserEdges comparedUser

        insertFeature featureMap fdc@(feature, _, _) = DM.insertWith
                (\_ old -> fdc : old)
                feature
                [fdc]
                featureMap

        addClassification c (f, v) = (f, v, c)

        usersMap = DM.fromList $ zipWith (\u t -> (name u, t)) users users

featurizeUser :: [Edge String FeatureValue] -> User -> [(Feature, FeatureValue)]
featurizeUser edges user =
        [ (UserPositiveEdges, fromIntegral positiveEdgesCount)
        , (UserNegativeEdges, fromIntegral $ edgeCount - positiveEdgesCount)
        , (UserTotalDegree  , fromIntegral edgeCount)
        , (UserAge          , fromIntegral $ age user)
        , (UserAccountAge   , fromIntegral $ accountAge user)
        , (UserKarma        , fromIntegral $ totalKarma user)
        ]
    where
        positiveEdgesCount = countPositiveEdges edges
        edgeCount          = length edges

featurizeComparedUser
        :: [Edge String FeatureValue] -> User -> [(Feature, FeatureValue)]
featurizeComparedUser edges user =
        [ (ComparedUserPositiveEdges, fromIntegral positiveEdgesCount)
        , ( ComparedUserNegativeEdges
          , fromIntegral $ edgeCount - positiveEdgesCount
          )
        , (ComparedUserTotalDegree, fromIntegral edgeCount)
        , (ComparedUserAccountAge , fromIntegral $ accountAge user)
        ]
    where
        positiveEdgesCount = countPositiveEdges edges
        edgeCount          = length edges

featurizeCommon
        :: [Edge String FeatureValue]
        -> User
        -> [Edge String FeatureValue]
        -> User
        -> [(Feature, FeatureValue)]
featurizeCommon userEdges user comparedUserEdges comparedUser =
        [ (AgeDifference, fromIntegral . abs $ age user - age comparedUser)
        , ( CommonNeighbors
          , fromIntegral . length $ DL.intersect userEdges comparedUserEdges
          )
        ]

countPositiveEdges :: [Edge String FeatureValue] -> Int
countPositiveEdges = length . filter isPositiveEdge
        where isPositiveEdge = (== Good) . classifyEdge

classifyEdge :: Edge String FeatureValue -> Classification
classifyEdge (Edge _ _ v) | v >= 0.8  = Excellent
                          | v >= 0.5  = Good
                          | v >= 0.2  = Neutral
                          | v >= 0.0  = Bad
                          | otherwise = Terrible

--TODO: actually use the correct distribuitions instead of forcefully transforming the data
--summarize and scoreNormal assume that the data is sufficient (ie every classification is present and every features has more than one non zero row)
summarize :: [[FeatureRow]] -> [(Classification, Count, [Summary])]
summarize allFeatures =
        [ ( classification
          , fromIntegral
                  . length $ head filtered
          , fmap summit filtered
          )
        | classification <- [minBound ..]
        , let
                filtered = filterClass classification allFeatures
        ]
    where
        filterClass classification =
                fmap (fmap extract . filter (sameClass classification))
        extract (feature, weight, _) = (feature, weight)

        sameClass classification (_, _, c) = c == classification

        summit features =
                let
                        count = fromIntegral $ length features
                        --TODO: we prolly want to use vectors all around?
                        values =
                                DVU.toList
                                        . DM.fromJust
                                        . SSN.standardize
                                        . DVU.fromList
                                        $ fmap snd features
                        mean = sum values / count
                in
                        Summary
                                { feature           = fst $ head features
                                --since we shaped the data to a normal distribuition the mean and deviation will not quite be 0 and 1
                                , mean              = mean
                                , standardDeviation =
                                                sqrt
                                                        ( sum
                                                                        [ ( v
                                                                          - mean
                                                                          )
                                                                                  ** 2.0
                                                                        | v <-
                                                                                values
                                                                        ]
                                                        / (count - 1)
                                                        )
                                }

scoreNormal
        :: Count
        -> [(Classification, Count, [Summary])]
        -> [(Feature, FeatureValue)]
        -> [(Classification, Double)]
scoreNormal totalCount summaries features = fmap foldScore summaries
    where
        foldScore (classification, count, summaries) =
                ( classification
                , foldl calculate (log (count / totalCount)) summaries + 1.0
                )

        featureMap = DM.fromList features
        -- log(x * y) = log(x) + log(y)
        calculate total Summary {..} =
                        total
                                + log
                                          (probabilityDensity
                                                  mean
                                                  standardDeviation
                                                  (featureMap ! feature)
                                          )

        probabilityDensity mean standardDeviation value =
                (1.0 / (sqrt (2.0 * pi) * standardDeviation))
                        * exp
                                  (-(  (value - mean)
                                    ** 2.0
                                    /  (2.0 * standardDeviation ** 2.0)
                                    )
                                  )