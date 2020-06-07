{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified Lib                           as L

main :: IO ()
main = do
        users <- L.createUsers 5
        chats <- L.createChats 20 users
        display users
        display chats
        where display = putStrLn . concatMap (("\n\n" ++) . show)
