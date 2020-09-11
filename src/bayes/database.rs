pub struct User<'lifetime> {
    pub id: u32,
    headline: &'lifetime str,
    age: u8,
    description: &'lifetime str,
    tags: Vec<&'lifetime str>,
    total_karma: u32,
    //  karmaStatus:: KarmaStatus,
    account_age: u16,
}

pub struct Chat {
    pub first_user_id: u32,
    pub second_user_id: u32,
    pub karma: i32,
    pub blocked: bool,
}

//to make it easier to cross validate
const age_range: (u8, u8) = (13, 100);
const new_user_account_age_range: (u16, u16) = (1, 30); // days
const new_user_karma_range: (i32, i32) = (10, 30 * 100); // 100 per day max

pub fn create_users() -> Vec<User<'static>> {
    return create_low_karma_new_users();
}

pub fn create_chats() -> Vec<Chat> {
    return create_chats_between_low_karma_new_user();
}

fn create_low_karma_new_users() -> Vec<User<'static>> {
    return vec![
        User { id: 1
            , headline: "mooooooooooooooooooooooooooo"
            , age: 13
            , description: "This is me boio. I like to graze and stomp"
            , tags: vec!["cows", "green pastures", "salt"]
            , total_karma: 5
            , account_age: 1
            }
        , User { id: 2
            , headline: "this is a headline"
            , age: 22
            , description: "This is a description!"
            , tags: vec!["Yeah", "yeah", "yeah", "yeah", "oh"]
            , total_karma: 200
            , account_age: 2
            }
        , User
            { id: 2
            , headline: "grab attention like hawk"
            , age: 24
            , description: "I will fill this later"
            , tags: vec![ "Run", "run", "run", "run", "run", "run", "run", "away", "oh", "oh", "oh", "oh"]
            , total_karma : 234
            , account_age : 3
            }
        , User { id: 4
            , headline: "who let the dogs out"
            , age: 87
            , description: "I say I will fill this later, but I never do"
            , tags: vec!["Fa-fa-fa-fa-fa-fa-fa-fa-fa-far", "better"]
            , total_karma : 400
            , account_age : 4
            }
        , User
            { id: 5
            , headline: "oi boio"
            , age: 18
            , description:
                    "I spread like strawberries, I climb like peas and beans"
            , tags: vec!["Qu'est-ce", "que", "c'est"]
            , total_karma : 23
            , account_age : 10
            }
        , User
            { id: 6
            , headline:
                    "this is my headline, there are many like it but this one is mine"
            , age: 25
            , description:
                    "But you know, I spread like strawberries, I climb like peas and beans"
            , tags: vec!["Psycho", "Killer"]
            , total_karma : 900
            , account_age : 20
            }
        , User
            { id: 7
            , headline: "who let the boios out?"
            , age: 37
            , description:
                    "praise the sun, git gud and engage in jolly cooperation"
            , tags: vec![ "Run", "run", "run", "run", "run", "run", "run", "away", "oh", "oh", "oh", "oh"]
            , total_karma : 3000
            , account_age : 30
            }
        , User
            { id: 8
            , headline: "but now I only move to move"
            , age: 28
            , description:
                    "Once upon a time, all the unicorns got away from the bulls"
            , tags: vec!["Fa-fa-fa-fa-fa-fa-fa-fa-fa-far", "better"]
            , total_karma : 826
            , account_age : 30
            }
        , User
            { id: 9
            , headline:
                    "this is some vague truism that I think is very deep and make me look very smart"
            , age: 19
            , description:
                    "More vague vaguery that I found intelectual. None of this has any depth and I think that philosophy is talking my ass off."
            , tags: vec!["Qu'est-ce", "que", "c'est"]
            , total_karma : 1600
            , account_age : 16
            }
        , User { id: 10
            , headline: "who is it? who is it, who?"
            , age: 28
            , description: "Ohhhhhh babe it is you!"
            , tags: vec!["Psycho", "Killer"]
            , total_karma : 60
            , account_age : 22
            }
    ];
}

fn create_chats_between_low_karma_new_user() -> Vec<Chat> {
    return vec![
        Chat {
            first_user_id: 1,
            second_user_id: 8,
            karma: 40,
            blocked: true,
        },
        Chat {
            first_user_id: 1,
            second_user_id: 5,
            karma: 15,
            blocked: false,
        },
        Chat {
            first_user_id: 1,
            second_user_id: 7,
            karma: 70,
            blocked: false,
        },
        Chat {
            first_user_id: 1,
            second_user_id: 3,
            karma: 74,
            blocked: false,
        },
        Chat {
            first_user_id: 2,
            second_user_id: 5,
            karma: 84,
            blocked: false,
        },
        Chat {
            first_user_id: 2,
            second_user_id: 6,
            karma: 35,
            blocked: false,
        },
        Chat {
            first_user_id: 2,
            second_user_id: 7,
            karma: 28,
            blocked: false,
        },
        Chat {
            first_user_id: 3,
            second_user_id: 6,
            karma: 86,
            blocked: false,
        },
        Chat {
            first_user_id: 3,
            second_user_id: 7,
            karma: 6,
            blocked: false,
        },
        Chat {
            first_user_id: 3,
            second_user_id: 5,
            karma: 29,
            blocked: false,
        },
        Chat {
            first_user_id: 4,
            second_user_id: 5,
            karma: 45,
            blocked: false,
        },
        Chat {
            first_user_id: 4,
            second_user_id: 6,
            karma: 53,
            blocked: false,
        },
        Chat {
            first_user_id: 4,
            second_user_id: 7,
            karma: 35,
            blocked: false,
        },
        Chat {
            first_user_id: 5,
            second_user_id: 7,
            karma: 15,
            blocked: false,
        },
        Chat {
            first_user_id: 5,
            second_user_id: 6,
            karma: 37,
            blocked: false,
        },
        Chat {
            first_user_id: 6,
            second_user_id: 7,
            karma: 67,
            blocked: false,
        },
        Chat {
            first_user_id: 7,
            second_user_id: 8,
            karma: 40,
            blocked: false,
        },
        Chat {
            first_user_id: 8,
            second_user_id: 5,
            karma: 17,
            blocked: false,
        },
        Chat {
            first_user_id: 8,
            second_user_id: 6,
            karma: 85,
            blocked: false,
        },
        Chat {
            first_user_id: 8,
            second_user_id: 4,
            karma: 9,
            blocked: false,
        },
        Chat {
            first_user_id: 8,
            second_user_id: 3,
            karma: 69,
            blocked: false,
        },
        Chat {
            first_user_id: 9,
            second_user_id: 7,
            karma: 49,
            blocked: true,
        },
        Chat {
            first_user_id: 9,
            second_user_id: 6,
            karma: 20,
            blocked: false,
        },
        Chat {
            first_user_id: 9,
            second_user_id: 4,
            karma: 79,
            blocked: false,
        },
        Chat {
            first_user_id: 9,
            second_user_id: 5,
            karma: 85,
            blocked: true,
        }
    ];
}

//    data KarmaStatus: Low | Medium | High deriving (Show, Eq, Ord)

//  data OnlineStatus = OnlineStatus {
//        userName:: String,
//        lastOnline:: Int
//  } deriving Show

//    manual

//    generated

//  createChats:: [User] -> IO [Chat]
//  createChats users = do
//        chatsPerUser <- normalRandom (0, size) size
//        pairs     <- mapM makeChatsForUser $ zip [0 .. size] chatsPerUser
//        mapM makeChat . filter sameUser . DL.nubBy sameChat $ concat pairs
//        where
//        size      = length users - 1
//        usersMap = DM.fromList $ zip [0 .. size] users

//        sameChat (u, u2) (t, t2) = u == t && u2 == t2 || u == t2 && u2 == t
//        sameUser (u, u2) = u /= u2

//        makeChatsForUser (current, total) =
//             map (current, ) <$> normalRandom (0, size) size

//        makeChat (i, i2) = do
//             let      firstUser  = usersMap ! i
//                  secondUser = usersMap ! i2
//             blocked <- (<= 5) <$> SR.randomRIO (1, 100:: Int)
//             karma    <- SR.randomRIO (5, 105:: Int)

//             pure Chat { first_user_id  = name firstUser
//, second_user_id = name secondUser
//, karma        = karma
//, blocked     = blocked
//}

//  normalRandom:: (Int, Int) -> Int -> IO [Int]
//  normalRandom (min, max) total = do
//        ps <- DRN.normalsIO' (0.5, 0.1):: IO [Double]
//        pure . take total $ map
//             (\p ->
//                  floor
//        ( p
//        * ( ( fromIntegral max
//             - fromIntegral min
//             )
//          + 1.0
//          )
//        )
//  + min
//             )
//             ps

//  takeRandom:: Random a => (a, a) -> Int -> IO [a]
//  takeRandom bounds total = do
//        generator <- SR.newStdGen
//        pure . take total $ SR.randomRs bounds generator

//    createChats:: Int -> [User] -> IO [Chat]
//    createChats total users = do
//         let size = length users - 1
//         numbers <- takeRandom (0, size) total
//         let      usersMap = DM.fromList $ zip [0 .. size] users
//              indexes =
//                    filter sameUser
//    . DL.nubBy sameChat
//    . zip numbers
//    $ reverse numbers
//         mapM (makeChat usersMap) indexes
//         where
//         sameChat (u, u2) (t, t2) = u == t && u2 == t2 || u == t2 && u2 == t
//         sameUser (u, u2) = u /= u2

//         makeChat usersMap (i, i2) = do
//              let      firstUser  = usersMap ! i
//                    secondUser = usersMap ! i2
//              blocked <- (== 31 ) <$> SR.randomRIO (1, 100:: Int)
//              karma    <- karmaByStatus
//                    $ min (karmaStatus firstUser) (karmaStatus secondUser)
//              pure Chat { first_user_id  = name firstUser
// , second_user_id = name secondUser
// , karma        = if blocked then 0 else max 10 $ div karma 10
// , blocked     = blocked
// }

//    createUsers:: Int -> IO [User]
//    createUsers total = do
//         totals <- splitFor total 3
//         createFor totals [createNewUsers, createMediumUsers, createOldUsers]

//    createNewUsers:: Int -> IO [User]
//    createNewUsers total = do
//         let tag = "new-user-"
//         totals <- splitFor total 2
//         users  <- createFor
//              totals
//              [createLowKarmaUsers tag, createMediumKarmaUsers tag]
//         mapM (makeaccount_age (0, newUserThreshold)) users

//    createMediumUsers:: Int -> IO [User]
//    createMediumUsers total = do
//         let tag = "medium-user-"
//         totals <- splitFor total 3
//         users  <- createFor
//              totals
//              [ createLowKarmaUsers tag
//              , createMediumKarmaUsers tag
//              , createHighKarmaUsers tag
//              ]
//         mapM (makeaccount_age (newUserThreshold + 1, mediumUserThreshold)) users

//    createOldUsers:: Int -> IO [User]
//    createOldUsers total = do
//         let tag = "old-user-"
//         totals <- splitFor total 2
//         users  <- createFor
//              totals
//              [createMediumKarmaUsers tag, createHighKarmaUsers tag]
//         mapM (makeaccount_age (mediumUserThreshold + 1, oldUserThreshold)) users

//    createLowKarmaUsers:: String -> Int -> IO [User]
//    createLowKarmaUsers tag = createKarma (tag ++ "low-karma-") Low

//    createMediumKarmaUsers:: String -> Int -> IO [User]
//    createMediumKarmaUsers tag = createKarma (tag ++ "medium-karma-") Medium

//    createHighKarmaUsers:: String -> Int -> IO [User]
//    createHighKarmaUsers tag = createKarma (tag ++ "high-karma-") High

//    createKarma:: String -> KarmaStatus -> Int -> IO [User]
//    createKarma tag status total = do
//         baseUsers <- makeUsers tag total
//         mapM (makeKarma status) baseUsers

//    createFor:: [Int] -> [Int -> IO [User]] -> IO [User]
//    createFor = go []
//         where
//         go users []         _     = pure users
//         go users (n: ns) (f: fs) = do
//              created <- f n
//              go (created ++ users) ns fs

//    splitFor:: Int -> Int -> IO [Int]
//    splitFor total = go [] total
//         where
//         go split current times
//              | current <= 0 || times == 0 = pure split
//              | otherwise = do
//                    let newCurrent = current - times
//                    n <- SR.randomRIO (1, newCurrent)
//                    go (split ++ [n]) (total - sum split) (times - 1)

//    makeaccount_age:: (Int, Int) -> User -> IO User
//    makeaccount_age bounds user = do
//         age <- normalRandom bounds
//         pure user { account_age = age }

//    makeKarma:: KarmaStatus -> User -> IO User
//    makeKarma status user = do
//         karma <- karmaByStatus status
//         pure user { total_karma = karma, karmaStatus = status }

//    we need it to be a normal distrubituion for the naive bayes
//    karmaByStatus:: KarmaStatus -> IO Int
//    karmaByStatus status = normalRandom $ case status of
//         Low     -> (lowKarma, mediumKarma)
//         Medium -> (mediumKarma, highKarma)
//         High    -> (highKarma, topKarma)

//    normalRandom:: (Int, Int) -> IO Int
//    normalRandom (min, max) = do
//         p <- DRN.normalIO' (0.5, 0.1):: IO Double
//         pure $ floor (p * ((fromIntegral max - fromIntegral min) + 1.0)) + min

//    makeUsers:: String -> Int -> IO [User]
//    makeUsers tag total = mapM make [1 .. total]
//         where
//         make n = do
//              description <- makeText (1, 20)
//              tags     <- makeTags
//              age      <- normalRandom (13, 99)
//              pure User { name     = tag <> show n
// , description = description
// , age      = age
// , tags     = tags
// , total_karma  = -1
// , account_age  = -1
// , karmaStatus = Low
// }

//         makeText bounds = do
//              total    <- SR.randomRIO bounds
//              letters <- takeRandom (1, 5) total
//              words    <- mapM (takeRandom ('a', 'z')) letters
//              pure $ unwords words

//         makeTags = do
//              total <- SR.randomRIO (1, 12)
//              CM.replicateM total $ makeText (1, 10)
