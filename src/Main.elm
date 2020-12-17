module Main exposing (..)

import Browser
import DailyTask exposing (Tasks)
import Html exposing (Html, div, form, h1, img, input, text)
import Html.Attributes exposing (src, type_, value)
import Html.Events exposing (onInput)
import Set exposing (Set)



---- MODEL ----


type alias Form =
    { title : String
    , description : String
    , start : String
    , end : String
    , weekday : Set Int
    }


emptyForm : Form
emptyForm =
    { title = ""
    , description = ""
    , start = ""
    , end = ""
    , weekday = Set.empty
    }


type alias Model =
    { currentTasks : Tasks
    , formData : Form
    }


init : ( Model, Cmd Msg )
init =
    ( { currentTasks = DailyTask.empty
      , formData = emptyForm
      }
    , Cmd.none
    )



---- UPDATE ----


type Field
    = Title
    | Description
    | Start
    | End


type Msg
    = UpdateField Field String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField field value ->
            ( { model
                | formData = updateForm model.formData field value
              }
            , Cmd.none
            )


updateForm : Form -> Field -> String -> Form
updateForm form field newValue =
    case field of
        Title ->
            { form | title = newValue }

        Description ->
            { form | description = newValue }

        Start ->
            { form | start = newValue }

        End ->
            { form | end = newValue }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewForm model ]


viewForm : Model -> Html Msg
viewForm { formData } =
    form []
        [ input
            [ value formData.title
            , type_ "text"
            , onInput (UpdateField Title)
            ]
            []
        , input
            [ value formData.description
            , type_ "text"
            , onInput (UpdateField Description)
            ]
            []
        , input
            [ value formData.start
            , type_ "time"
            , onInput (UpdateField Start)
            ]
            []
        , input
            [ value formData.end
            , type_ "time"
            , onInput (UpdateField End)
            ]
            []
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
