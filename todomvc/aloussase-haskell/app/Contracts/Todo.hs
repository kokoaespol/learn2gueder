{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}
module Contracts.Todo where

import           Data.Aeson
import           Data.Text            (Text)
import           Deriving.Aeson.Stock
import           GHC.Generics         (Generic)

data TodoView id = TodoView
    { todoViewId        :: id
    , todoViewContent   :: !Text
    , todoViewCompleted :: !Bool
    }
    deriving stock Generic
    deriving ToJSON via PrefixedSnake "todoView" (TodoView id)

-- | Payload used to create a new todo.
newtype CreateTodo = CreateTodo
    { createTodoContent :: Text
    }
    deriving stock Generic
    deriving (FromJSON) via PrefixedSnake "createTodo" CreateTodo

-- | Payload used to update a todo.
data PatchTodo = PatchTodo
    { patchTodoContent   :: !(Maybe Text)
    , patchTodoCompleted :: !(Maybe Bool)
    }
    deriving stock Generic
    deriving FromJSON via PrefixedSnake "patchTodo" PatchTodo


