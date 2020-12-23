module DailyTask exposing (Tasks, createTimeRange, empty, insertTask, parseTime)

import Dict exposing (Dict)
import Html.Attributes exposing (id)
import Set exposing (Set)


type alias ID =
    String


type alias Weekday =
    Int


type Time
    = Time
        { hour : Int
        , minute : Int
        }


type alias TimeRangeRecord =
    { start : Time
    , end : Time
    }


type TimeRange
    = TimeRange TimeRangeRecord


type alias TaskFields =
    { id : ID
    , title : String
    , description : String
    , weekdays : Set Weekday
    , time : TimeRange
    }


type Task
    = Task TaskFields


type alias TaskDictionary =
    Dict ID Task


type Tasks
    = Tasks TaskDictionary


empty : Tasks
empty =
    Tasks Dict.empty


createTimeRange : Time -> Time -> Maybe TimeRange
createTimeRange start end =
    if isBefore start end then
        Just (TimeRange { start = start, end = end })

    else
        Nothing


parseTime : String -> Maybe Time
parseTime time =
    case String.split ":" time of
        hourStr :: minuteStr :: [] ->
            case ( String.toInt hourStr, String.toInt minuteStr ) of
                ( Just hour, Just minute ) ->
                    if hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59 then
                        Just (Time { hour = hour, minute = minute })

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


findIntersections : TaskDictionary -> TimeRange -> Set Weekday -> TaskDictionary
findIntersections currentTasks time weekdays =
    currentTasks
        |> Dict.filter (isInSameDay weekdays)
        |> Dict.filter (doesIntersect time)


isInSameDay : Set Weekday -> ID -> Task -> Bool
isInSameDay weekdays _ (Task task) =
    Set.intersect task.weekdays weekdays
        |> Set.isEmpty


doesIntersect : TimeRange -> ID -> Task -> Bool
doesIntersect (TimeRange { start, end }) _ (Task { time }) =
    let
        (TimeRange taskTime) =
            time
    in
    isBefore end taskTime.start
        || isAfter start taskTime.end
        || end
        == taskTime.start
        || start
        == taskTime.end


doesNotIntersect : TimeRange -> Task -> Bool
doesNotIntersect (TimeRange { start, end }) (Task { time }) =
    let
        (TimeRange taskTime) =
            time
    in
    (isBefore start taskTime.start && (isBefore end taskTime.end || end == taskTime.end))
        || (isAfter start taskTime.start || start == taskTime.start && isAfter end taskTime.end)


isBefore : Time -> Time -> Bool
isBefore (Time fstTime) (Time sndTime) =
    if fstTime.hour < sndTime.hour then
        True

    else if fstTime.hour == sndTime.hour && fstTime.minute < sndTime.minute then
        True

    else
        False


isSameTime : Time -> Time -> Bool
isSameTime fstTime sndTime =
    fstTime == sndTime


isAfter : Time -> Time -> Bool
isAfter fstTime sndTime =
    not (isSameTime fstTime sndTime || isBefore fstTime sndTime)


makeWeekday : Int -> Maybe Weekday
makeWeekday weekday =
    if weekday > 1 && weekday < 8 then
        Just weekday

    else
        Nothing


insertTask : Tasks -> TaskFields -> Result Tasks Tasks
insertTask (Tasks currentTasks) newTask =
    let
        intersections =
            findIntersections currentTasks newTask.time newTask.weekdays
    in
    if Dict.isEmpty intersections then
        Dict.insert newTask.id (Task newTask) currentTasks
            |> Tasks
            |> Ok

    else
        Err (Tasks intersections)
