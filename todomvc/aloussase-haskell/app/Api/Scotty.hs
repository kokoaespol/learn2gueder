{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Api.Scotty where

import           Control.Monad.IO.Class
import           Data.Aeson                            hiding (json)
import           Web.Scotty

import           Contracts.Todo
import           Control.Monad.Catch
import           Data.Function                         ((&))
import qualified Data.Text.Lazy                        as TL
import           Database.Persist.Sql                  (toSqlKey)
import           Model.Todo                            (Todo (..))
import qualified Model.Todo                            as Todo
import           Network.HTTP.Types
import           Network.Wai                           (Application)
import           Network.Wai.Middleware.ProblemDetails (problemDetails404,
                                                        setDetail,
                                                        throwProblemDetailsIO)

instance Parsable (Todo.Key Todo) where
    parseParam param = Right $ toSqlKey (read . TL.unpack $ param)

api :: IO Application
api = scottyApp $ do
    delete "/todos/:id" $ do
        todoId <- param "id"
        Todo.delete todoId
        status noContent204

    post "/todos" $ do
        payload <- jsonData
        todo <- Todo.create (createTodoContent payload) False
        status created201
        json todo

    patch "/todos/:id" $ do
        todoId <- param "id"
        payload <- jsonData
        updatedTodo <- handle (\(_ :: SomeException) ->
                        liftIO $ throwProblemDetailsIO (problemDetails404 & setDetail "Todo with that id not found")) $ do
            mapM_ (Todo.updateContent todoId) (patchTodoContent payload)
            mapM (Todo.updateCompleted todoId) (patchTodoCompleted payload)
        json updatedTodo

    get "/todos" $ json =<< liftIO Todo.findAll
