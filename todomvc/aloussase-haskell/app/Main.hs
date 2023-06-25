module Main where

import           Database.Persist.Sqlite
import           Network.Wai.Handler.Warp              (run)
import           Network.Wai.Middleware.ProblemDetails (problemDetails)
import           Network.Wai.Middleware.Static

import qualified Api.Scotty                            as Scotty
import qualified Api.Servant                           as Servant
import qualified Model.Todo                            as Todo

staticBase = "/home/aloussase/Code/learn2gueder/todomvc/aloussase-haskell/frontend/public"

main :: IO ()
main = do
    api <- pure Servant.api
    Todo.createTable
    run 3000 $ staticPolicy (addBase staticBase) $ problemDetails api

