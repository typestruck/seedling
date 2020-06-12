{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad(void)

import qualified Lib                           as L

main :: IO ()
main = do
        users <- L.createUsers 5
        chats <- L.createChats 20 users
        let network = L.createNetwork chats
        display users
        display chats
        print network
        void $ L.createSuggestions users network $ L.name (users !! 0)

        where display = putStrLn . concatMap ( ( ++ "\n\n") . show)
