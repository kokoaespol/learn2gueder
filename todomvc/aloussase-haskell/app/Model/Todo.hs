{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Model.Todo where

import           Control.Monad.IO.Class
import           Data.Aeson              (ToJSON)
import           Data.Text               (Text)
import           Database.Persist        as Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics            (Generic)

import           Contracts.Todo

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    content Text
    completed Bool
    deriving Generic
|]

instance ToJSON Todo

todoDbName :: Text
todoDbName = "todo.db"

createTable :: (MonadIO m) => m ()
createTable = liftIO $ runSqlite todoDbName $ runMigration migrateAll

create :: (MonadIO m) => Text -> Bool -> m (TodoView (Key Todo))
create content completed = do
    todoId <- liftIO $ runSqlite todoDbName $ insert (Todo content completed)
    return $ TodoView todoId content completed

findAll :: (MonadIO m) => m [TodoView (Key Todo)]
findAll = do
    entities <- liftIO $ runSqlite todoDbName $ selectList [] []
    return $ map mkTodoView entities
    where
        mkTodoView entity =
            let todoId = entityKey entity
                todo :: Todo = entityVal entity
             in
                TodoView todoId (todoContent todo) (todoCompleted todo)

delete :: (MonadIO m) => Key Todo -> m ()
delete todoId = liftIO $ runSqlite todoDbName $ Persist.delete todoId

updateContent :: (MonadIO m) => Key Todo -> Text -> m (TodoView (Key Todo))
updateContent todoId newContent = do
    newTodo <- liftIO $ runSqlite todoDbName $ updateGet todoId [TodoContent =. newContent]
    return $ TodoView
                todoId
                (todoContent newTodo)
                (todoCompleted newTodo)

updateCompleted :: (MonadIO m) => Key Todo -> Bool -> m (TodoView (Key Todo))
updateCompleted todoId newCompleted = do
    newTodo <- liftIO $ runSqlite todoDbName $ updateGet todoId [TodoCompleted =. newCompleted]
    return $ TodoView
                todoId
                (todoContent newTodo)
                (todoCompleted newTodo)
