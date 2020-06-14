{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Control.Monad                  ( void )

import qualified Lib                           as L

main :: IO ()
main = do
        users <- L.createUsers 5
        chats <- L.createChats 20 users
        let network = L.createNetwork chats
        display users
        display chats
        print network
        putStrLn "\n"
        let features = L.featurize network users
        print features
        putStrLn "\n"
        let
                summary = L.summarize
                        (fromIntegral . length $ head features)
                        features
        print summary
        putStrLn "\n"
        suggestions <-
                L.createSuggestions
                        (fromIntegral . length $ head features)
                        summary
                . L.name
                $ head users
        print suggestions
        where display = putStrLn . concatMap ((++ "\n\n") . show)
