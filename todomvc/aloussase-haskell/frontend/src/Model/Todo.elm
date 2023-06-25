module Model.Todo exposing (Todo, decode)

import Json.Decode as D


type alias Todo =
    { id : Int
    , content : String
    , completed : Bool
    }


decode : D.Decoder Todo
decode =
    D.map3
        Todo
        (D.field "id" D.int)
        (D.field "content" D.string)
        (D.field "completed" D.bool)
