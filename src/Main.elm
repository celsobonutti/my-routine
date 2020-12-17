module Main exposing (..)

import Browser
import DailyTask exposing (Tasks)
import Html exposing (Html, button, div, form, h1, img, input, label, text)
import Html.Attributes exposing (checked, for, name, src, type_, value)
import Html.Events exposing (onCheck, onInput)
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
    | CheckDay Int Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField field value ->
            ( { model
                | formData = updateForm model.formData field value
              }
            , Cmd.none
            )

        CheckDay day isChecked ->
            ( { model
                | formData = updateCheck model.formData day isChecked
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


updateCheck : Form -> Int -> Bool -> Form
updateCheck ({ weekday } as form) day isChecked =
    let
        newDays =
            if isChecked then
                Set.insert day weekday

            else
                Set.remove day weekday
    in
    { form | weekday = newDays }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewForm model ]


viewForm : Model -> Html Msg
viewForm ({ formData } as model) =
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
        , div [] (viewCheckboxes model)
        , button
            []
            [ text "Criar Tarefa" ]
        ]


viewCheckboxes : Model -> List (Html Msg)
viewCheckboxes model =
    List.range 0 6
        |> List.map (viewCheckbox model)


viewCheckbox : Model -> Int -> Html Msg
viewCheckbox { formData } num =
    let
        isChecked =
            Set.member num formData.weekday

        dayName =
            numberToWeekday num
    in
    div []
        [ input
            [ type_ "checkbox"
            , checked isChecked
            , name ("check" ++ dayName)
            , onCheck (CheckDay num)
            ]
            []
        , label [ for ("check" ++ dayName) ] [ text dayName ]
        ]


numberToWeekday : Int -> String
numberToWeekday day =
    case day of
        0 ->
            "Domingo"

        1 ->
            "Segunda"

        2 ->
            "Terça"

        3 ->
            "Quarta"

        4 ->
            "Quinta"

        5 ->
            "Sexta"

        6 ->
            "Sábado"

        _ ->
            "Dia inválido"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
