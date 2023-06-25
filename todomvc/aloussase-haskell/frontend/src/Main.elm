module Main exposing (main)

import Api.Todo
import Browser
import Html exposing (Html, div, h1, i, input, li, span, text, ul)
import Html.Attributes exposing (checked, class, classList, disabled, placeholder, type_, value)
import Html.Events as E
import Model.Todo exposing (Todo)
import Msg exposing (..)


type alias Model =
    { todos : List Todo
    , errMsg : Maybe String
    , newTodoContent : String
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialState =
            { todos = []
            , errMsg = Nothing
            , newTodoContent = ""
            }
    in
    ( initialState, Api.Todo.getAll )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "TodoMVC" ]
        , viewErrMsg model.errMsg
        , div [ class "create-todo" ]
            [ input [ type_ "text", placeholder "New todo", E.onInput GotInput, value model.newTodoContent ] []
            , input [ type_ "submit", value "Create", E.onClick CreateTodo ] []
            ]
        , ul [ class "todos" ] (List.map viewTodo model.todos)
        ]


viewErrMsg : Maybe String -> Html Msg
viewErrMsg errMsg =
    div [ classList [ ( "error-msg", isJust errMsg ) ] ]
        [ text (errMsg |> Maybe.withDefault "") ]


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        _ ->
            False


viewTodo : Todo -> Html Msg
viewTodo todo =
    li [ class "todo" ]
        [ input [ type_ "checkbox", checked todo.completed, E.onCheck (CompleteTodo todo.id) ] []
        , input
            [ type_ "text"
            , value todo.content
            , class "todo-content"
            , disabled True
            ]
            []
        , div [ class "todo-actions" ]
            [ span
                [ class "delete-todo", E.onClick (DeleteTodo todo.id) ]
                [ i [ class "fa fa-times" ] [] ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTodos (Ok todos) ->
            ( { model | todos = todos }, Cmd.none )

        GotTodos (Err _) ->
            ( { model | errMsg = Just "Failed to retrieve ToDo's" }, Cmd.none )

        GotInput input ->
            ( { model | newTodoContent = input }, Cmd.none )

        CreateTodo ->
            if String.isEmpty model.newTodoContent then
                ( model, Cmd.none )

            else
                ( model, Api.Todo.create model.newTodoContent )

        CreatedTodo (Ok todo) ->
            ( { model | todos = todo :: model.todos, newTodoContent = "" }, Cmd.none )

        CreatedTodo (Err _) ->
            ( { model | errMsg = Just "Failed to create ToDo" }, Cmd.none )

        DeleteTodo todoId ->
            ( model, Api.Todo.delete todoId )

        DeletedTodo todoId (Ok _) ->
            ( { model
                | todos = List.filter (\todo -> todo.id /= todoId) model.todos
              }
            , Cmd.none
            )

        DeletedTodo _ (Err _) ->
            ( { model | errMsg = Just "Failed to delete ToDo" }, Cmd.none )

        CompleteTodo todoId completed ->
            ( model, Api.Todo.complete todoId completed )

        CompletedTodo _ _ (Err _) ->
            ( { model | errMsg = Just "Failed to complete todo" }, Cmd.none )

        CompletedTodo todoId completed (Ok _) ->
            let
                newTodos =
                    List.map
                        (\todo ->
                            if todo.id == todoId then
                                { todo | completed = completed }

                            else
                                todo
                        )
                        model.todos
            in
            ( { model | todos = newTodos }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
