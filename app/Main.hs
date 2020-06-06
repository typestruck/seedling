module Main where

import qualified Lib as L

main :: IO ()
main = do
        users <- L.createUsers 10
        let usersDisplay = map ('\n' (:) . show) users
        putStrLn usersDisplay
