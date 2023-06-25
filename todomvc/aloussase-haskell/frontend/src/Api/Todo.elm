module Api.Todo exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E
import Model.Todo as Todo
import Msg exposing (Msg(..))


create : String -> Cmd Msg
create content =
    Http.post
        { url = "/todos"
        , body = Http.jsonBody (E.object [ ( "content", E.string content ) ])
        , expect = Http.expectJson CreatedTodo Todo.decode
        }


getAll : Cmd Msg
getAll =
    Http.get
        { url = "/todos"
        , expect = Http.expectJson GotTodos (D.list Todo.decode)
        }


delete : Int -> Cmd Msg
delete todoId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/todos/" ++ String.fromInt todoId
        , body = Http.emptyBody
        , expect = Http.expectWhatever (DeletedTodo todoId)
        , timeout = Nothing
        , tracker = Nothing
        }


complete : Int -> Bool -> Cmd Msg
complete todoId completed =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "/todos/" ++ String.fromInt todoId
        , body = Http.jsonBody (E.object [ ( "completed", E.bool completed ) ])
        , expect = Http.expectWhatever (CompletedTodo todoId completed)
        , timeout = Nothing
        , tracker = Nothing
        }


updateContent : Int -> String -> Cmd Msg
updateContent todoId newContent =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "/todos/" ++ String.fromInt todoId
        , body = Http.jsonBody (E.object [ ( "content", E.string newContent ) ])
        , expect = Http.expectWhatever (ContentUpdated newContent)
        , timeout = Nothing
        , tracker = Nothing
        }
