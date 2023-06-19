module Main where

import           Database.Persist.Sqlite
import           Network.Wai.Handler.Warp              (run)
import           Network.Wai.Middleware.ProblemDetails (problemDetails)

import qualified Api.Scotty                            as Scotty
import qualified Model.Todo                            as Todo

main :: IO ()
main = do
    api <- Scotty.api
    Todo.createTable
    run 3000 $ problemDetails api

