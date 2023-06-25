module Msg exposing (Msg(..))

import Http
import Model.Todo exposing (Todo)


type Msg
    = GotTodos (Result Http.Error (List Todo))
    | CreateTodo
    | CreatedTodo (Result Http.Error Todo)
    | GotInput String
    | DeleteTodo Int
    | DeletedTodo Int (Result Http.Error ())
    | CompleteTodo Int Bool
    | CompletedTodo Int Bool (Result Http.Error ())
    | EditTodo Int
    | UpdateTodoContent
    | ContentUpdated String (Result Http.Error ())
