module Main exposing (..)

import Browser
import DailyTask exposing (Tasks, insertTask)
import Html exposing (Html, button, div, form, h1, img, input, label, text)
import Html.Attributes exposing (checked, class, for, name, src, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
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
    , currentUuid : Maybe Uuid.Uuid
    , currentSeed : Seed
    }


init : ( Int, List Int ) -> ( Model, Cmd Msg )
init ( seed, seedExtension ) =
    ( { currentTasks = DailyTask.empty
      , formData = emptyForm
      , currentSeed = initialSeed seed seedExtension
      , currentUuid = Nothing
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
    | InsertTask


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

        InsertTask ->
            ( insertTask model
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


insertTask : Model -> Model
insertTask ({ formData, currentTasks, currentSeed } as model) =
    let
        ( newUuid, newSeed ) =
            step Uuid.generator currentSeed

        newTasks =
            DailyTask.parseTime formData.start
                |> Result.andThen
                    (\startTime ->
                        DailyTask.parseTime formData.end
                            |> Result.andThen
                                (\endTime ->
                                    DailyTask.createTimeRange startTime endTime
                                )
                    )
                |> Result.andThen
                    (\timeRange ->
                        DailyTask.insertTask currentTasks <|
                            { id = Uuid.toString newUuid
                            , title =
                                formData.title
                            , description = formData.description
                            , weekdays = formData.weekday
                            , time = timeRange
                            }
                    )
    in
    case newTasks of
        Ok tasks ->
            { model | currentUuid = Just newUuid, currentSeed = currentSeed, currentTasks = tasks }

        Err _ ->
            model



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewForm model ]


viewForm : Model -> Html Msg
viewForm ({ formData } as model) =
    form [ class "task-form", onSubmit InsertTask ]
        [ input
            [ value formData.title
            , type_ "text"
            , onInput (UpdateField Title)
            , class "task-form__input"
            ]
            []
        , input
            [ value formData.description
            , type_ "text"
            , onInput (UpdateField Description)
            , class "task-form__input"
            ]
            []
        , input
            [ value formData.start
            , type_ "time"
            , onInput (UpdateField Start)
            , class "task-form__input task-form__input--time"
            ]
            []
        , input
            [ value formData.end
            , type_ "time"
            , onInput (UpdateField End)
            , class "task-form__input task-form__input--time"
            ]
            []
        , div [ class "task-form__checkboxes" ] (viewCheckboxes model)
        , button
            [ class "task-form__button" ]
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


main : Program ( Int, List Int ) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
