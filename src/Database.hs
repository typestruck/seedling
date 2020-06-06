{-# LANGUAGE DuplicateRecordFields #-}

module Database where

import qualified System.Random                 as SR
import System.Random(Random(..))
import qualified Control.Monad as CM

data User = User {
        name :: String,
        description :: String,
        age :: Int,
        description :: String,
        tags :: [String],
        totalKarma :: Int,
        accountAge :: Int
}

data Chat = Chat {
        firstUserName :: String,
        secondUserName :: String,
        karma :: Int
}

data OnlineStatus = OnlineStatus {
        userName :: String,
        lastOnline :: Int
}

-- | New users are up to a month
newUserThreshold = 60 * 60 * 60 * 24 * 30
-- | Medium users are up to a year
mediumUserThreshold = newUserThreshold * 12
oldUserThreshold = newUserThreshold * 10

lowKarma = 5
mediumKarma = 5000
highKarma = 25000
topKarma = 300000

createUsers :: Int -> IO [User]
createUsers total = do
        totals <- splitFor total 3
        createFor totals [createNewUsers, createMediumUsers, createOldUsers]

createNewUsers :: Int -> IO [User]
createNewUsers total = do
        let tag = "new-user-"
        totals <- splitFor total 2
        users <- createFor totals [createLowKarmaUsers tag, createMediumKarmaUsers tag]
        mapM (makeAccountAge (0, newUserThreshold)) users

createMediumUsers :: Int -> IO [User]
createMediumUsers total = do
        let tag = "medium-user-"
        totals <- splitFor total 3
        users <- createFor totals [createLowKarmaUsers tag, createMediumKarmaUsers tag, createHighKarmaUsers tag]
        mapM (makeAccountAge (newUserThreshold + 1, mediumUserThreshold)) users

createOldUsers :: Int -> IO [User]
createOldUsers total = do
        let tag = "old-user-"
        totals <- splitFor total 2
        users <- createFor totals [createMediumKarmaUsers tag, createHighKarmaUsers tag]
        mapM (makeAccountAge (mediumUserThreshold + 1, oldUserThreshold)) users

createLowKarmaUsers :: String -> Int -> IO [User]
createLowKarmaUsers tag = createKarma (tag ++ "low-karma-") (mediumKarma, highKarma)

createMediumKarmaUsers :: String -> Int -> IO [User]
createMediumKarmaUsers tag = createKarma (tag ++ "medium-karma-") (mediumKarma, highKarma)

createHighKarmaUsers :: String -> Int -> IO [User]
createHighKarmaUsers tag = createKarma (tag ++ "high-karma-") (highKarma, topKarma)

createKarma :: String -> (Int, Int) -> Int -> IO [User]
createKarma tag bounds total = do
        baseUsers <- makeUsers tag total
        mapM (makeKarma bounds) baseUsers

createFor :: [Int] -> [Int -> IO [User]] -> IO [User]
createFor = go []
    where
        go users []       _        = pure users
        go users (n : ns) (f : fs) = do
                created <- f n
                go (created ++ users) ns fs

splitFor :: Int -> Int -> IO [Int]
splitFor total = go [] total
    where
        go split current times
                | current <= 0 || times == 0 = pure split
                | otherwise = do
                        let newCurrent = total - times - current
                        n <- SR.randomRIO (1, newCurrent)
                        go (split ++ [n]) (newCurrent - n) (times - 1)

makeAccountAge :: (Int, Int) -> User -> IO User
makeAccountAge bounds user = do
        age <- SR.randomRIO bounds
        pure user { accountAge = age }

makeKarma :: (Int, Int) -> User -> IO User
makeKarma bounds user = do
        karma <- SR.randomRIO bounds
        pure user { totalKarma = karma }

makeUsers :: String -> Int -> IO [User]
makeUsers tag total = mapM make [1..total]
    where
        make n = do
                description <- makeText (1, 300)
                tags        <- makeTags
                age         <- SR.randomRIO (13, 99)
                pure User { name        = tag <> show n
                          , description = description
                          , age         = age
                          , tags        = tags
                          , totalKarma  = -1
                          , accountAge  = -1
                          }

        makeText bounds = do
                total     <- SR.randomRIO bounds
                letters   <- takeRandom (1, 10) total
                words     <- mapM (takeRandom ('a', 'z')) letters
                pure $ unwords words

        takeRandom bounds total = do
                generator <- SR.newStdGen
                pure . take total $ SR.randomRs bounds generator

        makeTags = do
                total <- SR.randomRIO (1, 12)
                CM.replicateM total $ makeText (1, 10)
