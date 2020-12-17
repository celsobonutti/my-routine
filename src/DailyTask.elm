module DailyTask exposing (empty, insertTask, createTime, createTimeRange, Tasks)

import Dict exposing (Dict)
import Html.Attributes exposing (id)
import Set exposing (Set)


type alias ID =
    String


type alias Weekday =
    Int


type alias Time =
    { hour : Int
    , minute : Int
    }


type alias TimeRange =
    { start : Time
    , end : Time
    }


type alias Task =
    { id : ID
    , title : String
    , weekdays : Set Weekday
    , time : TimeRange
    }

type alias TaskDictionary
    = Dict ID Task

type Tasks =
    Tasks TaskDictionary

empty : Tasks
empty =
    Tasks (Dict.empty)

createTimeRange : Time -> Time -> Maybe TimeRange
createTimeRange start end =
    if isBefore start end then
        Just { start = start, end = end }
    else
        Nothing

createTime : Int -> Int -> Maybe Time
createTime hour minute =
    if hour >= 0 && hour < 24 && minute >= 0 && minute < 60 then
        Just { hour = hour, minute = minute }

    else
        Nothing


findIntersections : TaskDictionary -> TimeRange -> Set Weekday -> TaskDictionary
findIntersections currentTasks time weekdays =
    currentTasks
        |> Dict.filter (isInSameDay weekdays)
        |> Dict.filter (doesIntersect time)


isInSameDay : Set Weekday -> ID -> Task -> Bool
isInSameDay weekdays _ task =
    Set.intersect task.weekdays weekdays
        |> Set.isEmpty


doesIntersect : TimeRange -> ID -> Task -> Bool
doesIntersect { start, end } _ { time } =
    isBefore end time.start
    || isAfter start time.end
    || end == time.start
    || start == time.end


doesNotIntersect : TimeRange -> Task -> Bool
doesNotIntersect { start, end } { time } =
    (isBefore start time.start && (isBefore end time.end || end == time.end))
        || (isAfter start time.start || start == time.start && isAfter end time.end)


isBefore : Time -> Time -> Bool
isBefore fstTime sndTime =
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


insertTask : Tasks -> String -> TimeRange -> Set Weekday -> ID -> Result Tasks Tasks
insertTask (Tasks currentTasks) title timeRange weekdays id =
    let
        intersections =
            findIntersections currentTasks timeRange weekdays
    in
    if Dict.isEmpty intersections then
        Dict.insert id { id = id
        , title = title
        , time = timeRange
        , weekdays = weekdays
        } currentTasks
        |> Tasks |> Ok
    else
        Err (Tasks intersections)
