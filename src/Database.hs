{-# LANGUAGE DuplicateRecordFields, LambdaCase #-}

module Database
        ( User(..)
        , Chat(..)
        , OnlineStatus(..)
        , createUsers
        , createChats
        )
where

import qualified System.Random                 as SR
import           System.Random                  ( Random(..) )
import qualified Control.Monad                 as CM
import qualified Data.Map                      as DM
import           Data.Map                       ( (!) )
import qualified Data.List                     as DL
import qualified Debug.Trace                   as DT

data KarmaStatus = Low | Medium | High deriving (Show, Eq, Ord)

data User = User {
        name :: String,
        description :: String,
        age :: Int,
        description :: String,
        tags :: [String],
        totalKarma :: Int,
        karmaStatus :: KarmaStatus,
        accountAge :: Int
} deriving Show

data Chat = Chat {
        firstUserName :: String,
        secondUserName :: String,
        karma :: Int
} deriving Show

data OnlineStatus = OnlineStatus {
        userName :: String,
        lastOnline :: Int
} deriving Show

-- | New users are up to a month
newUserThreshold = 60 * 60 * 60 * 24 * 30
-- | Medium users are up to a year
mediumUserThreshold = newUserThreshold * 12
oldUserThreshold = newUserThreshold * 10

lowKarma = 5
mediumKarma = 5000
highKarma = 25000
topKarma = 300000

createChats :: Int -> [User] -> IO [Chat]
createChats total users = do
        let size = length users - 1
        numbers <- takeRandom (0, size) total
        let     usersMap = DM.fromList $ zip [0 .. size] users
                indexes  = DL.nubBy sameChat . zip numbers $ reverse numbers
        mapM (makeChat usersMap) indexes
    where
        sameChat (u, u2) (t, t2) = u == t && u2 == t2 || u == t2 && u2 == t

        makeChat usersMap (i, i2) = do
                let     firstUser  = usersMap ! i
                        secondUser = usersMap ! i2
                karma <- karmaByStatus
                        $ min (karmaStatus firstUser) (karmaStatus secondUser)
                pure Chat { firstUserName  = name firstUser
                          , secondUserName = name secondUser
                          , karma          = div karma 10
                          }

createUsers :: Int -> IO [User]
createUsers total = do
        totals <- splitFor total 3
        createFor totals [createNewUsers, createMediumUsers, createOldUsers]

createNewUsers :: Int -> IO [User]
createNewUsers total = do
        let tag = "new-user-"
        totals <- splitFor total 2
        users  <- createFor
                totals
                [createLowKarmaUsers tag, createMediumKarmaUsers tag]
        mapM (makeAccountAge (0, newUserThreshold)) users

createMediumUsers :: Int -> IO [User]
createMediumUsers total = do
        let tag = "medium-user-"
        totals <- splitFor total 3
        users  <- createFor
                totals
                [ createLowKarmaUsers tag
                , createMediumKarmaUsers tag
                , createHighKarmaUsers tag
                ]
        mapM (makeAccountAge (newUserThreshold + 1, mediumUserThreshold)) users

createOldUsers :: Int -> IO [User]
createOldUsers total = do
        let tag = "old-user-"
        totals <- splitFor total 2
        users  <- createFor
                totals
                [createMediumKarmaUsers tag, createHighKarmaUsers tag]
        mapM (makeAccountAge (mediumUserThreshold + 1, oldUserThreshold)) users

createLowKarmaUsers :: String -> Int -> IO [User]
createLowKarmaUsers tag = createKarma (tag ++ "low-karma-") Low

createMediumKarmaUsers :: String -> Int -> IO [User]
createMediumKarmaUsers tag = createKarma (tag ++ "medium-karma-") Medium

createHighKarmaUsers :: String -> Int -> IO [User]
createHighKarmaUsers tag = createKarma (tag ++ "high-karma-") High

createKarma :: String -> KarmaStatus -> Int -> IO [User]
createKarma tag status total = do
        baseUsers <- makeUsers tag total
        mapM (makeKarma status) baseUsers

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
                        let newCurrent = current - times
                        n <- SR.randomRIO (1, newCurrent)
                        go (split ++ [n]) (total - sum split) (times - 1)

makeAccountAge :: (Int, Int) -> User -> IO User
makeAccountAge bounds user = do
        age <- SR.randomRIO bounds
        pure user { accountAge = age }

makeKarma :: KarmaStatus -> User -> IO User
makeKarma status user = do
        karma <- karmaByStatus status
        pure user { totalKarma = karma, karmaStatus = status }

karmaByStatus :: KarmaStatus -> IO Int
karmaByStatus = SR.randomRIO . \case
        Low    -> (lowKarma, mediumKarma)
        Medium -> (mediumKarma, highKarma)
        High   -> (highKarma, topKarma)

makeUsers :: String -> Int -> IO [User]
makeUsers tag total = mapM make [1 .. total]
    where
        make n = do
                description <- makeText (1, 20)
                tags        <- makeTags
                age         <- SR.randomRIO (13, 99)
                pure User { name        = tag <> show n
                          , description = description
                          , age         = age
                          , tags        = tags
                          , totalKarma  = -1
                          , accountAge  = -1
                          , karmaStatus = Low
                          }

        makeText bounds = do
                total   <- SR.randomRIO bounds
                letters <- takeRandom (1, 5) total
                words   <- mapM (takeRandom ('a', 'z')) letters
                pure $ unwords words

        makeTags = do
                total <- SR.randomRIO (1, 12)
                CM.replicateM total $ makeText (1, 10)

takeRandom :: Random a => (a, a) -> Int -> IO [a]
takeRandom bounds total = do
        generator <- SR.newStdGen
        pure . take total $ SR.randomRs bounds generator
