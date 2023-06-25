module Main exposing (main)

import Api.Todo
import Browser
import Html exposing (Html, div, h1, i, input, li, span, text, ul)
import Html.Attributes exposing (checked, class, classList, disabled, placeholder, type_, value)
import Html.Events as E
import Model.Todo exposing (Todo)
import Msg exposing (..)


type State
    = Normal
    | Editing Int


type alias Model =
    { todos : List Todo
    , errMsg : Maybe String
    , newTodoContent : String
    , state : State
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
            , state = Normal
            }
    in
    ( initialState, Api.Todo.getAll )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "TodoMVC" ]
        , viewErrMsg model.errMsg
        , div [ class "create-todo" ]
            [ input
                [ type_ "text"
                , placeholder (inputPlaceholder model)
                , E.onInput GotInput
                , value model.newTodoContent
                ]
                []
            , input
                [ type_ "submit"
                , value (submitValue model)
                , E.onClick (submitClickEvent model)
                ]
                []
            ]
        , ul [ class "todos" ] (List.map viewTodo model.todos)
        ]


submitValue : Model -> String
submitValue model =
    case model.state of
        Normal ->
            "Create"

        Editing _ ->
            "Update"


submitClickEvent : Model -> Msg
submitClickEvent model =
    case model.state of
        Normal ->
            CreateTodo

        Editing _ ->
            UpdateTodoContent


inputPlaceholder : Model -> String
inputPlaceholder model =
    case model.state of
        Normal ->
            "New todo"

        Editing _ ->
            "Edit todo"


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
                [ class "edit-todo", E.onClick (EditTodo todo.id) ]
                [ i [ class "fa fa-pencil" ] [] ]
            , span
                [ class "delete-todo", E.onClick (DeleteTodo todo.id) ]
                [ i [ class "fa fa-times" ] [] ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.state, msg ) of
        ( _, GotTodos (Ok todos) ) ->
            ( { model | todos = todos }, Cmd.none )

        ( _, GotTodos (Err _) ) ->
            ( { model | errMsg = Just "Failed to retrieve ToDo's" }, Cmd.none )

        ( _, GotInput input ) ->
            ( { model | newTodoContent = input }, Cmd.none )

        ( Normal, CreateTodo ) ->
            if String.isEmpty model.newTodoContent then
                ( model, Cmd.none )

            else
                ( model, Api.Todo.create model.newTodoContent )

        ( Normal, CreatedTodo (Ok todo) ) ->
            ( { model | todos = todo :: model.todos, newTodoContent = "" }, Cmd.none )

        ( Normal, CreatedTodo (Err _) ) ->
            ( { model | errMsg = Just "Failed to create ToDo" }, Cmd.none )

        ( Normal, DeleteTodo todoId ) ->
            ( model, Api.Todo.delete todoId )

        ( Normal, DeletedTodo todoId (Ok _) ) ->
            ( { model
                | todos = List.filter (\todo -> todo.id /= todoId) model.todos
              }
            , Cmd.none
            )

        ( Normal, DeletedTodo _ (Err _) ) ->
            ( { model | errMsg = Just "Failed to delete ToDo" }, Cmd.none )

        ( _, CompleteTodo todoId completed ) ->
            ( model, Api.Todo.complete todoId completed )

        ( _, CompletedTodo _ _ (Err _) ) ->
            ( { model | errMsg = Just "Failed to complete todo" }, Cmd.none )

        ( _, CompletedTodo todoId completed (Ok _) ) ->
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

        ( Normal, EditTodo todoId ) ->
            ( { model
                | state = Editing todoId
                , newTodoContent =
                    List.filter (\todo -> todo.id == todoId) model.todos
                        |> List.head
                        |> Maybe.map (\todo -> todo.content)
                        |> Maybe.withDefault ""
              }
            , Cmd.none
            )

        ( Editing todoId, UpdateTodoContent ) ->
            ( model, Api.Todo.updateContent todoId model.newTodoContent )

        ( Editing todoId, ContentUpdated newContent (Ok _) ) ->
            ( { model
                | state = Normal
                , newTodoContent = ""
                , todos =
                    List.map
                        (\todo ->
                            if todo.id == todoId then
                                { todo | content = newContent }

                            else
                                todo
                        )
                        model.todos
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
