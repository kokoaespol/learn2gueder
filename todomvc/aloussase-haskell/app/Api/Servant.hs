{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Api.Servant (api) where

import           Contracts.Todo
import           Control.Exception                     (SomeException)
import           Control.Monad.Catch                   (handle)
import           Control.Monad.IO.Class                (liftIO)
import           Data.Function                         ((&))
import           Data.Functor                          (($>))
import           Network.Wai.Middleware.ProblemDetails
import           Servant

import           Model.Todo                            (Key, Todo (..))
import qualified Model.Todo                            as Todo

type Api =
    "todos" :>
        (    Get '[JSON] [TodoView (Key Todo)]
        :<|> ReqBody '[JSON] CreateTodo :> PostCreated '[JSON] (TodoView (Key Todo))
        :<|> Capture "id" (Key Todo) :> ReqBody '[JSON] PatchTodo :> Patch '[JSON] (Maybe (TodoView (Key Todo)))
        :<|> Capture "id" (Key Todo) :> DeleteNoContent
        )

api :: Application
api = serve (Proxy :: Proxy Api) server

server :: Server Api
server = todoGet
    :<|> todoCreate
    :<|> todoPatch
    :<|> todoDelete

    where
        todoGet = Todo.findAll

        todoDelete todoId = Todo.delete todoId $> NoContent

        todoCreate payload = Todo.create (createTodoContent payload) False

        todoPatch todoId payload = do
            handle (\(_ :: SomeException) -> liftIO $ throwProblemDetailsIO todoNotFound) $ do
                mapM_ (Todo.updateContent todoId) (patchTodoContent payload)
                mapM (Todo.updateCompleted todoId) (patchTodoCompleted payload)

        todoNotFound = problemDetails404 & setDetail "Todo with that id not found"
