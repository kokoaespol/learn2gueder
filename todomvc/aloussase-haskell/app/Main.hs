{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text  (Text)
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    delete "/todos/:id" $ do
        undefined

    post "/todos" $ do
        undefined

    patch "/todos/:id" $ do
        undefined

    get "/todos" $ do
        text "Hello, world!"
