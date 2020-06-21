{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Database where

import qualified System.Random                 as SR
import           System.Random                  ( Random(..) )
import qualified Control.Monad                 as CM
import qualified Data.HashMap.Lazy             as DM
import           Data.HashMap.Lazy              ( (!) )
import qualified Data.List                     as DL
import qualified Debug.Trace                   as DT
import qualified Data.Random.Normal            as DRN

-- data KarmaStatus = Low | Medium | High deriving (Show, Eq, Ord)

data User = User {
        name :: String,
        headline :: String,
        age :: Int,
        description :: String,
        tags :: [String],
        totalKarma :: Int,
      --  karmaStatus :: KarmaStatus,
        accountAge :: Int
} deriving (Show,Eq)

data Chat = Chat {
        firstUserName :: String,
        secondUserName :: String,
        karma :: Int,
        blocked :: Bool
} deriving Show

data OnlineStatus = OnlineStatus {
        userName :: String,
        lastOnline :: Int
} deriving Show

-- manual

-- I can't seem to face up to the facts
-- I'm tense and nervous and I can't relax
-- I can't sleep 'cause my bed's on fire
-- Don't touch me I'm a real live wire
-- Psycho Killer
-- Qu'est-ce que c'est
-- Fa-fa-fa-fa-fa-fa-fa-fa-fa-far better
-- Run, run, run, run, run, run, run away oh oh
-- Psycho Killer
-- Qu'est-ce que c'est
-- Fa-fa-fa-fa-fa-fa-fa-fa-fa-far better
-- Run, run, run, run, run, run, run away oh oh oh oh
-- Yeah yeah yeah yeah!
-- You start a conversation you can't even finish it
-- You're talking a lot, but you're not saying anything
-- When I have nothing to say, my lips are sealed
-- Say something once, why say it again?
-- Psycho Killer
-- Qu'est-ce que c'est
-- Fa-fa-fa-fa-fa-fa-fa-fa-fa-far better
-- Run, run, run, run, run, run, run away oh oh oh
-- Psycho Killer
-- Qu'est-ce que c'est
-- Fa-fa-fa-fa-fa-fa-fa-fa-fa-far better
-- Run, run, run, run, run, run, run away oh oh oh oh
-- Yeah yeah yeah yeah
-- Ce que j'ai fais, ce soir la
-- Ce qu'elle a dit, ce soir la
-- Realisant mon espoir
-- Je me lance, vers la gloire, OK
-- Yeah, yeah, yeah, yeah, yeah, yeah, yeah, yeah, yeah, yeah
-- We are vain and we are blind
-- I hate people when they're not polite

--to make it easier to cross validate
ageRange = (13, 100)
newUserAccountAgeRange = (1, 30) -- days
newUserKarmaRange = (10, 30 * 100) -- 100 per day max

users :: [User]
users = lowKarmaNewUsers

lowKarmaNewUsers :: [User]
lowKarmaNewUsers =
        [ User { name        = "low karma new user A"
               , headline    = "mooooooooooooooooooooooooooo"
               , age         = 13
               , description = "This is me boio. I like to graze and stomp"
               , tags        = ["cows", "green pastures", "salt"]
               , totalKarma  = 5
               , accountAge  = 1
               }
        , User { name        = "low karma new user B"
               , headline    = "this is a headline"
               , age         = 22
               , description = "This is a description!"
               , tags        = ["Yeah", "yeah", "yeah", "yeah", "oh"]
               , totalKarma  = 200
               , accountAge  = 2
               }
        , User
                { name        = "low karma new user C"
                , headline    = "grab attention like hawk"
                , age         = 24
                , description = "I will fill this later"
                , tags        = [ "Run"
                                , "run"
                                , "run"
                                , "run"
                                , "run"
                                , "run"
                                , "run"
                                , "away"
                                , "oh"
                                , "oh"
                                , "oh"
                                , "oh"
                                ]
                , totalKarma  = 234
                , accountAge  = 3
                }
        , User { name        = "low karma new user D"
               , headline    = "who let the dogs out"
               , age         = 87
               , description = "I say I will fill this later, but I never do"
               , tags        = ["Fa-fa-fa-fa-fa-fa-fa-fa-fa-far", "better"]
               , totalKarma  = 400
               , accountAge  = 4
               }
        , User
                { name        = "low karma new user E"
                , headline    = "oi boio"
                , age         = 18
                , description =
                        "I spread like strawberries, I climb like peas and beans"
                , tags        = ["Qu'est-ce", "que", "c'est"]
                , totalKarma  = 23
                , accountAge  = 10
                }
        , User
                { name        = "low karma new user F"
                , headline    =
                        "this is my headline, there are many like it but this one is mine"
                , age         = 25
                , description =
                        "But you know, I spread like strawberries, I climb like peas and beans"
                , tags        = ["Psycho", "Killer"]
                , totalKarma  = 900
                , accountAge  = 20
                }
        , User
                { name        = "low karma new user G"
                , headline    = "who let the boios out?"
                , age         = 37
                , description =
                        "praise the sun, git gud and engage in jolly cooperation"
                , tags        = [ "Run"
                                , "run"
                                , "run"
                                , "run"
                                , "run"
                                , "run"
                                , "run"
                                , "away"
                                , "oh"
                                , "oh"
                                , "oh"
                                , "oh"
                                ]
                , totalKarma  = 3000
                , accountAge  = 30
                }
        , User
                { name        = "low karma new user H"
                , headline    = "but now I only move to move"
                , age         = 28
                , description =
                        "Once upon a time, all the unicorns got away from the bulls"
                , tags        = ["Fa-fa-fa-fa-fa-fa-fa-fa-fa-far", "better"]
                , totalKarma  = 826
                , accountAge  = 30
                }
        , User
                { name        = "low karma new user I"
                , headline    =
                        "this is some vague truism that I think is very deep and make me look very smart"
                , age         = 19
                , description =
                        "More vague vaguery that I found intelectual. None of this has any depth and I think that philosophy is talking my ass off."
                , tags        = ["Qu'est-ce", "que", "c'est"]
                , totalKarma  = 1600
                , accountAge  = 16
                }
        , User { name        = "low karma new user J"
               , headline    = "who is it? who is it, who?"
               , age         = 28
               , description = "Ohhhhhh babe it is you!"
               , tags        = ["Psycho", "Killer"]
               , totalKarma  = 60
               , accountAge  = 22
               }
        ]

chats :: [Chat]
chats = chatsBetweenLowKarmaNewUser

chatsBetweenLowKarmaNewUser =
        [ Chat { firstUserName  = "low karma new user A"
               , secondUserName = "low karma new user H"
               , karma          = 40
               , blocked        = True
               }
        , Chat { firstUserName  = "low karma new user A"
               , secondUserName = "low karma new user E"
               , karma          = 15
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user A"
               , secondUserName = "low karma new user G"
               , karma          = 70
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user A"
               , secondUserName = "low karma new user C"
               , karma          = 74
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user B"
               , secondUserName = "low karma new user E"
               , karma          = 84
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user B"
               , secondUserName = "low karma new user F"
               , karma          = 35
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user B"
               , secondUserName = "low karma new user G"
               , karma          = 28
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user C"
               , secondUserName = "low karma new user F"
               , karma          = 86
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user C"
               , secondUserName = "low karma new user G"
               , karma          = 6
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user C"
               , secondUserName = "low karma new user E"
               , karma          = 29
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user D"
               , secondUserName = "low karma new user E"
               , karma          = 45
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user D"
               , secondUserName = "low karma new user F"
               , karma          = 53
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user D"
               , secondUserName = "low karma new user G"
               , karma          = 35
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user E"
               , secondUserName = "low karma new user G"
               , karma          = 15
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user E"
               , secondUserName = "low karma new user F"
               , karma          = 37
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user F"
               , secondUserName = "low karma new user G"
               , karma          = 67
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user G"
               , secondUserName = "low karma new user H"
               , karma          = 40
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user H"
               , secondUserName = "low karma new user E"
               , karma          = 17
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user H"
               , secondUserName = "low karma new user F"
               , karma          = 85
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user H"
               , secondUserName = "low karma new user D"
               , karma          = 9
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user H"
               , secondUserName = "low karma new user C"
               , karma          = 69
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user I"
               , secondUserName = "low karma new user G"
               , karma          = 49
               , blocked        = True
               }
        , Chat { firstUserName  = "low karma new user I"
               , secondUserName = "low karma new user F"
               , karma          = 20
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user I"
               , secondUserName = "low karma new user D"
               , karma          = 79
               , blocked        = False
               }
        , Chat { firstUserName  = "low karma new user I"
               , secondUserName = "low karma new user E"
               , karma          = 85
               , blocked        = True
               }
        ]

-- generated

createChats :: [User] -> IO [Chat]
createChats users = do
        chatsPerUser <- normalRandom (0, size) size
        pairs        <- mapM makeChatsForUser $ zip [0 .. size] chatsPerUser
        mapM makeChat . filter sameUser . DL.nubBy sameChat $ concat pairs
    where
        size     = length users - 1
        usersMap = DM.fromList $ zip [0 .. size] users

        sameChat (u, u2) (t, t2) = u == t && u2 == t2 || u == t2 && u2 == t
        sameUser (u, u2) = u /= u2

        makeChatsForUser (current, total) =
                map (current, ) <$> normalRandom (0, size) size

        makeChat (i, i2) = do
                let     firstUser  = usersMap ! i
                        secondUser = usersMap ! i2
                blocked <- (<= 5) <$> SR.randomRIO (1, 100 :: Int)
                karma   <- SR.randomRIO (5, 105 :: Int)

                pure Chat { firstUserName  = name firstUser
                          , secondUserName = name secondUser
                          , karma          = karma
                          , blocked        = blocked
                          }

normalRandom :: (Int, Int) -> Int -> IO [Int]
normalRandom (min, max) total = do
        ps <- DRN.normalsIO' (0.5, 0.1) :: IO [Double]
        pure . take total $ map
                (\p ->
                        floor
                                        ( p
                                        * ( ( fromIntegral max
                                            - fromIntegral min
                                            )
                                          + 1.0
                                          )
                                        )
                                + min
                )
                ps

takeRandom :: Random a => (a, a) -> Int -> IO [a]
takeRandom bounds total = do
        generator <- SR.newStdGen
        pure . take total $ SR.randomRs bounds generator

-- createChats :: Int -> [User] -> IO [Chat]
-- createChats total users = do
--         let size = length users - 1
--         numbers <- takeRandom (0, size) total
--         let     usersMap = DM.fromList $ zip [0 .. size] users
--                 indexes =
--                         filter sameUser
--                                 . DL.nubBy sameChat
--                                 . zip numbers
--                                 $ reverse numbers
--         mapM (makeChat usersMap) indexes
--     where
--         sameChat (u, u2) (t, t2) = u == t && u2 == t2 || u == t2 && u2 == t
--         sameUser (u, u2) = u /= u2

--         makeChat usersMap (i, i2) = do
--                 let     firstUser  = usersMap ! i
--                         secondUser = usersMap ! i2
--                 blocked <- (== 31 ) <$> SR.randomRIO (1, 100 :: Int)
--                 karma   <- karmaByStatus
--                         $ min (karmaStatus firstUser) (karmaStatus secondUser)
--                 pure Chat { firstUserName  = name firstUser
--                           , secondUserName = name secondUser
--                           , karma          = if blocked then 0 else max 10 $ div karma 10
--                           , blocked        = blocked
--                           }

-- createUsers :: Int -> IO [User]
-- createUsers total = do
--         totals <- splitFor total 3
--         createFor totals [createNewUsers, createMediumUsers, createOldUsers]

-- createNewUsers :: Int -> IO [User]
-- createNewUsers total = do
--         let tag = "new-user-"
--         totals <- splitFor total 2
--         users  <- createFor
--                 totals
--                 [createLowKarmaUsers tag, createMediumKarmaUsers tag]
--         mapM (makeAccountAge (0, newUserThreshold)) users

-- createMediumUsers :: Int -> IO [User]
-- createMediumUsers total = do
--         let tag = "medium-user-"
--         totals <- splitFor total 3
--         users  <- createFor
--                 totals
--                 [ createLowKarmaUsers tag
--                 , createMediumKarmaUsers tag
--                 , createHighKarmaUsers tag
--                 ]
--         mapM (makeAccountAge (newUserThreshold + 1, mediumUserThreshold)) users

-- createOldUsers :: Int -> IO [User]
-- createOldUsers total = do
--         let tag = "old-user-"
--         totals <- splitFor total 2
--         users  <- createFor
--                 totals
--                 [createMediumKarmaUsers tag, createHighKarmaUsers tag]
--         mapM (makeAccountAge (mediumUserThreshold + 1, oldUserThreshold)) users

-- createLowKarmaUsers :: String -> Int -> IO [User]
-- createLowKarmaUsers tag = createKarma (tag ++ "low-karma-") Low

-- createMediumKarmaUsers :: String -> Int -> IO [User]
-- createMediumKarmaUsers tag = createKarma (tag ++ "medium-karma-") Medium

-- createHighKarmaUsers :: String -> Int -> IO [User]
-- createHighKarmaUsers tag = createKarma (tag ++ "high-karma-") High

-- createKarma :: String -> KarmaStatus -> Int -> IO [User]
-- createKarma tag status total = do
--         baseUsers <- makeUsers tag total
--         mapM (makeKarma status) baseUsers

-- createFor :: [Int] -> [Int -> IO [User]] -> IO [User]
-- createFor = go []
--     where
--         go users []       _        = pure users
--         go users (n : ns) (f : fs) = do
--                 created <- f n
--                 go (created ++ users) ns fs

-- splitFor :: Int -> Int -> IO [Int]
-- splitFor total = go [] total
--     where
--         go split current times
--                 | current <= 0 || times == 0 = pure split
--                 | otherwise = do
--                         let newCurrent = current - times
--                         n <- SR.randomRIO (1, newCurrent)
--                         go (split ++ [n]) (total - sum split) (times - 1)

-- makeAccountAge :: (Int, Int) -> User -> IO User
-- makeAccountAge bounds user = do
--         age <- normalRandom bounds
--         pure user { accountAge = age }

-- makeKarma :: KarmaStatus -> User -> IO User
-- makeKarma status user = do
--         karma <- karmaByStatus status
--         pure user { totalKarma = karma, karmaStatus = status }

-- --we need it to be a normal distrubituion for the naive bayes
-- karmaByStatus :: KarmaStatus -> IO Int
-- karmaByStatus status = normalRandom $ case status of
--         Low    -> (lowKarma, mediumKarma)
--         Medium -> (mediumKarma, highKarma)
--         High   -> (highKarma, topKarma)

-- normalRandom :: (Int, Int) -> IO Int
-- normalRandom (min, max) = do
--         p <- DRN.normalIO' (0.5, 0.1) :: IO Double
--         pure $ floor (p * ((fromIntegral max - fromIntegral min) + 1.0)) + min

-- makeUsers :: String -> Int -> IO [User]
-- makeUsers tag total = mapM make [1 .. total]
--     where
--         make n = do
--                 description <- makeText (1, 20)
--                 tags        <- makeTags
--                 age         <- normalRandom (13, 99)
--                 pure User { name        = tag <> show n
--                           , description = description
--                           , age         = age
--                           , tags        = tags
--                           , totalKarma  = -1
--                           , accountAge  = -1
--                           , karmaStatus = Low
--                           }

--         makeText bounds = do
--                 total   <- SR.randomRIO bounds
--                 letters <- takeRandom (1, 5) total
--                 words   <- mapM (takeRandom ('a', 'z')) letters
--                 pure $ unwords words

--         makeTags = do
--                 total <- SR.randomRIO (1, 12)
--                 CM.replicateM total $ makeText (1, 10)
