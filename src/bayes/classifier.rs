use petgraph::graph::{UnGraph, NodeIndex, Edges};
use petgraph::Undirected;
use rusty_machine::learning::naive_bayes::{Gaussian, NaiveBayes};
use rusty_machine::linalg::Matrix;
use rusty_machine::learning::SupModel;
use fnv::FnvHashMap;

use crate::database::{Chat, User};

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Classification {
    Terrible,
    Bad,
    Neutral,
    Good,
    Excellent,
}

//this needs to be reevaluated to find actual distrubuitions
enum Feature {
    UserPositiveEdges,
    ComparedUserPositiveEdges ,
    UserNegativeEdges ,
    ComparedUserNegativeEdges ,
    CommonNeighbors ,
    UserTotalDegree ,
    ComparedUserTotalDegree ,
    // UserPageRank Int , // this is category data!
    // ComparedUserPageRank Int // this is category data!
    UserAge ,
    AgeDifference ,
    UserAccountAge ,
    ComparedUserAccountAge ,
    UserKarma
    // CosineSimilarityDescriptions , // this is binary data!
    // CosineSimilarityTags Double // this is binary data!
}

const number_of_features : usize = 12;
const number_of_classes : usize = 5;
const good_chat_score : f64 = 0.5;
const bad_chat_score : f64 = 0.0;

pub fn featurize(network : &UnGraph::<u32, f64>, chats : &[Chat], users: &[User]) -> (Matrix<f64>, Matrix<f64>) {
    let mut users_map : FnvHashMap<u32, &User> = FnvHashMap::default();

    for usr in users {
        users_map.insert(usr.id, usr);
    }

    let mut features = Vec::new();
    //each row tells what class the respective row in the feature matrix belongs to
    let mut classes = Vec::new();

    for cht in chats {
        let user_edges = network.edges(NodeIndex::new(cht.first_user_id as usize));
        let compared_user_edges = network.edges(NodeIndex::new(cht.second_user_id as usize));

        //UserPositiveEdges
        features.push(count_positive_edges(&user_edges));
        //ComparedUserPositiveEdges
        features.push(count_positive_edges(&compared_user_edges));
        //UserNegativeEdges
        features.push(count_negative_edges(&user_edges));
        //ComparedUserNegativeEdges
        features.push(count_negative_edges(&compared_user_edges));
        // CommonNeighbors

        // UserTotalDegree ,
        // ComparedUserTotalDegree ,
        // // UserPageRank Int , // this is category data!
        // // ComparedUserPageRank Int // this is category data!
        // UserAge ,
        // AgeDifference ,
        // UserAccountAge ,
        // ComparedUserAccountAge ,
        // UserKarma
        // // CosineSimilarityDescriptions , // this is binary data!
        // // CosineSimilarityTags Double // this is binary data!

    }

    return (Matrix::new(features.len() / number_of_features, number_of_features, features), Matrix::new(classes.len() / number_of_classes, number_of_classes, classes));
}

fn count_positive_edges(edges : &Edges<f64, Undirected>) -> f64 {
    return edges.filter(|edge| *edge.weight() >= good_chat_score).count() as f64;
}

fn count_negative_edges(edges : &Edges<f64, Undirected>) -> f64 {
    return edges.filter(|edge| *edge.weight() <= bad_chat_score).count() as f64;
}

// type FeatureRow = (Feature, FeatureValue, Classification)

// //this can be improved to not repeat values
// featurize :: UGraph String Double -> [Chat] -> [User] -> [[FeatureRow]]
// featurize network chats users = DM.elems $ foldl build DM.empty chats
//     where
//         build featureMap Chat {..} =
//                 let
//                         (user, comparedUser) =
//                                 ( usersMap ! firstUserName
//                                 , usersMap ! secondUserName
//                                 )
//                         (userEdges, comparedUserEdges) =
//                                 ( DGU.incidentEdges network $ name user
//                                 , DGU.incidentEdges network $ name comparedUser
//                                 )

//                         classification = classifyEdge . DMY.fromJust $ DL.find
//                                 (\(Edge _ b v) -> b == name comparedUser)
//                                 userEdges
//                 in
//                         foldl insertFeature featureMap
//                         .  fmap (addClassification classification)
//                         $  featurizeCommon userEdges
//                                            user
//                                            comparedUserEdges
//                                            comparedUser
//                         ++ featurizeUser userEdges user
//                         ++ featurizeComparedUser comparedUserEdges comparedUser

//         insertFeature featureMap fdc@(feature, _, _) = DM.insertWith
//                 (\_ old -> fdc : old)
//                 feature
//                 [fdc]
//                 featureMap

//         addClassification c (f, v) = (f, v, c)

//         usersMap = DM.fromList $ zipWith (\u t -> (name u, t)) users users

// featurizeUser :: [Edge String FeatureValue] -> User -> [(Feature, FeatureValue)]
// featurizeUser edges user =
//         [ (UserPositiveEdges, fromIntegral positiveEdgesCount)
//         , (UserNegativeEdges, fromIntegral $ edgeCount - positiveEdgesCount)
//         , (UserTotalDegree  , fromIntegral edgeCount)
//         , (UserAge          , fromIntegral $ age user)
//         , (UserAccountAge   , fromIntegral $ accountAge user)
//         , (UserKarma        , fromIntegral $ totalKarma user)
//         ]
//     where
//         positiveEdgesCount = countPositiveEdges edges
//         edgeCount          = length edges

// featurizeComparedUser
//         :: [Edge String FeatureValue] -> User -> [(Feature, FeatureValue)]
// featurizeComparedUser edges user =
//         [ (ComparedUserPositiveEdges, fromIntegral positiveEdgesCount)
//         , ( ComparedUserNegativeEdges
//           , fromIntegral $ edgeCount - positiveEdgesCount
//           )
//         , (ComparedUserTotalDegree, fromIntegral edgeCount)
//         , (ComparedUserAccountAge , fromIntegral $ accountAge user)
//         ]
//     where
//         positiveEdgesCount = countPositiveEdges edges
//         edgeCount          = length edges

// featurizeCommon
//         :: [Edge String FeatureValue]
//         -> User
//         -> [Edge String FeatureValue]
//         -> User
//         -> [(Feature, FeatureValue)]
// featurizeCommon userEdges user comparedUserEdges comparedUser =
//         [ (AgeDifference, fromIntegral . abs $ age user - age comparedUser)
//         , ( CommonNeighbors
//           , fromIntegral . length $ DL.intersect userEdges comparedUserEdges
//           )
//         ]

// countPositiveEdges :: [Edge String FeatureValue] -> Int
// countPositiveEdges = length . filter isPositiveEdge
//         where isPositiveEdge = (== Good) . classifyEdge

// classifyEdge :: Edge String FeatureValue -> Classification
// classifyEdge (Edge _ _ v) , v >= 0.8  = Excellent
//                           , v >= 0.5  = Good
//                           , v >= 0.2  = Neutral
//                           , v >= 0.0  = Bad
//                           , otherwise = Terrible

// //TEST WITH THE IRIS PETAL DATABASE

// //MUST INCLUDE THE SUMMARY OF ALL FEATURES
// //IT ACTUALLY HAS TO USE THE CORRECT DISTRIBUTION
// summarize :: [[FeatureRow]] -> [(Classification, Count, [Maybe Summary])]
// summarize allFeatures = filter
//         tooSmall
//         [ ( classification
//           , fromIntegral . length $ head filtered
//           , fmap summit filtered
//           )
//         , classification <- [minBound ..]
//         , let filtered = filterClass classification allFeatures
//         ]
//     where
//         tooSmall (_, count, _) = count > 1
//         filterClass classification =
//                 fmap (fmap extract . filter (sameClass classification))
//         extract (feature, weight, _) = (feature, weight)

//         sameClass classification (_, _, c) = c == classification

//         summit features =
//                 //TODO: we prolly want to use vectors all around?
//                 case Just $ fmap snd features of
//                         Nothing -> Nothing
//                         Just v ->
//                                 let
//                                         values =  v
//                                         count  = fromIntegral $ length features
//                                         mean   = sum values / count
//                                 in
//                                         //since we shaped the data to a normal distribuition the mean and deviation will not quite be 0 and 1
//                                         Just $ Summary
//                                                 { feature = fst $ head features
//                                                 , mean = mean
//                                                 , standardDeviation =
//                                                         sqrt
//                                                                 ( sum
//                                                                                 [ (v
//                                                                                   - mean
//                                                                                   )
//                                                                                           ** 2.0
//                                                                                 , v <-
//                                                                                         values
//                                                                                 ]
//                                                                 / ( count
//                                                                   - 1
//                                                                   )
//                                                                 )
//                                                 }

// //LOG AND SMOOTHING CAN BE ADDED AFTER MAKING SURE IT IS CORRECT

// scoreNormal
//         :: Count
//         -> [(Classification, Count, [Maybe Summary])]
//         -> [(Feature, FeatureValue)]
//         -> [(Classification, Double)]
// scoreNormal totalCount summaries features = fmap foldScore summaries
//     where
//         foldScore (classification, count, summaries) =
//                 ( classification
//                 , foldl calculate ((count / totalCount)) summaries
//                 )

//         featureMap = DM.fromList features
//         // log(x * y) = log(x) + log(y)
//         calculate total summary = total *
//                 (case summary of
//                         Nothing           -> smoothing
//                         Just Summary {..} -> probabilityDensity
//                                 mean
//                                 standardDeviation
//                                 (featureMap ! feature)
//                 )

//         probabilityDensity mean standardDeviation value =
//                 (1.0 / (sqrt (2.0 * pi) * standardDeviation))
//                         * exp
//                                   (-(  (value - mean)
//                                     ** 2.0
//                                     /  (2.0 * standardDeviation ** 2.0)
//                                     )
//                                   )

//         smoothing = 1

// t = 10.0

// test1 =
//         [ [ (UserPositiveEdges, 3.393533211, Good)
//           , (UserPositiveEdges, 3.110073483, Good)
//           , (UserPositiveEdges, 1.343808831, Good)
//           , (UserPositiveEdges, 3.582294042, Good)
//           , (UserPositiveEdges, 2.280362439, Good)
//           , (UserPositiveEdges, 7.423436942, Bad)
//           , (UserPositiveEdges, 5.745051997, Bad)
//           , (UserPositiveEdges, 9.172168622, Bad)
//           , (UserPositiveEdges, 7.792783481, Bad)
//           , (UserPositiveEdges, 7.939820817, Bad)
//           ]
//         , [ (UserNegativeEdges, 2.331273381, Good)
//           , (UserNegativeEdges, 1.781539638, Good)
//           , (UserNegativeEdges, 3.368360954, Good)
//           , (UserNegativeEdges, 4.67917911 , Good)
//           , (UserNegativeEdges, 2.866990263, Good)
//           , (UserNegativeEdges, 4.696522875, Bad)
//           , (UserNegativeEdges, 3.533989803, Bad)
//           , (UserNegativeEdges, 2.511101045, Bad)
//           , (UserNegativeEdges, 3.424088941, Bad)
//           , (UserNegativeEdges, 0.791637231, Bad)
//           ]
//         ]

// f1 = [(UserPositiveEdges, 3.393533211), (UserNegativeEdges, 2.331273381)]
