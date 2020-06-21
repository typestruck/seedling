{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Control.Monad                  ( void )

import qualified Lib                           as L
import qualified Database                      as D

main :: IO ()
main = do
        let suggestions = L.createSuggestions D.chats D.users $ head D.users
        print suggestions
