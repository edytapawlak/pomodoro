module Main exposing (Msg(..), Timer, init, main, step, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type State
    = LongB
    | ShortB
    | Task


type alias Timer =
    { minutes : Int
    , seconds : Int
    , state : Maybe State
    , succesed : Int
    , sub : Bool
    , focusTime : Int
    , taskTime : Int
    , shortBreakTime : Int
    , longBreakTime : Int
    }


init : () -> ( Timer, Cmd Msg )
init _ =
    ( Timer 0 0 Nothing 0 False 0 25 4 10
    , Cmd.none
    )



-- UPDATE


type Msg
    = Start
    | Stop
    | Continue
    | Reset
    | LongBreak
    | ShortBreak
    | Step Time.Posix
    | UpdateTaskTime String
    | UpdateLongTime String
    | UpdateShortTime String


update : Msg -> Timer -> ( Timer, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( setTime model.taskTime
                0
                { model
                    | sub = True
                    , state = Just Task
                }
            , Cmd.none
            )

        Stop ->
            ( { model | sub = False }
            , Cmd.none
            )

        Continue ->
            ( { model | sub = True }
            , Cmd.none
            )

        Reset ->
            ( { model | minutes = 25, seconds = 0 }
            , Cmd.none
            )

        LongBreak ->
            ( setTime model.longBreakTime
                0
                { model | state = Just LongB, succesed = model.succesed - 3 }
            , Cmd.none
            )

        ShortBreak ->
            ( setTime model.shortBreakTime
                0
                { model | state = Just ShortB, succesed = model.succesed - 1 }
            , Cmd.none
            )

        Step newTime ->
            case model.state of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                _ ->
                    ( step model
                    , Cmd.none
                    )

        UpdateTaskTime newTime ->
            let
                time =
                    String.toInt newTime
            in
            case time of
                Nothing ->
                    ( { model | taskTime = 0 }, Cmd.none )

                Just t ->
                    ( { model | taskTime = t }
                    , Cmd.none
                    )

        UpdateLongTime newTime ->
            let
                time =
                    String.toInt newTime
            in
            case time of
                Nothing ->
                    ( { model | longBreakTime = 0 }, Cmd.none )

                Just t ->
                    ( { model | longBreakTime = t }
                    , Cmd.none
                    )

        UpdateShortTime newTime ->
            let
                time =
                    String.toInt newTime
            in
            case time of
                Nothing ->
                    ( { model | shortBreakTime = 0 }, Cmd.none )

                Just t ->
                    ( { model | shortBreakTime = t }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Timer -> Sub Msg
subscriptions model =
    if model.sub then
        Time.every 1000 Step

    else
        Sub.none


step : Timer -> Timer
step timer =
    if timer.seconds == 0 then
        if timer.minutes == 0 then
            if timer.state == Just Task then
                succesTimer timer.taskTime <| resetTimer timer

            else
                resetTimer timer

        else
            { timer
                | minutes = timer.minutes - 1
                , seconds = 59
            }

    else
        { timer | seconds = timer.seconds - 1 }


resetTimer : Timer -> Timer
resetTimer t =
    { t
        | minutes = 0
        , seconds = 0
        , state = Nothing
    }


succesTimer : Int -> Timer -> Timer
succesTimer n t =
    { t
        | succesed = t.succesed + 1
        , focusTime = t.focusTime + n
    }


setTime : Int -> Int -> Timer -> Timer
setTime min sec t =
    { t | minutes = min, seconds = sec }



-- VIEW


view : Timer -> Html Msg
view model =
    let
        minute =
            printTime model.minutes

        second =
            printTime model.seconds
    in
    div []
        [ div
            []
            [ h1 [] [ text (minute ++ ":" ++ second) ]
            , button [ onClick <| Start ] [ text "Start" ]
            , button [ onClick Stop ] [ text "Stop" ]
            , button [ onClick Continue ] [ text "Continue" ]
            ]
        , div
            []
            [ button [ onClick <| ShortBreak, Attr.disabled (not (model.succesed >= 1)) ] [ text "Short break " ]
            , button [ onClick <| LongBreak, Attr.disabled (not (model.succesed >= 3)) ] [ text "Long break" ]
            , h2 [] [ text <| "Your points: " ++ String.fromInt model.succesed ]
            , h2 [] [ text <| "You staied focused for " ++ String.fromInt model.focusTime ++ " minutes" ]
            ]
        , div []
            [ h1 [] [ text "Settings " ]
            , text "Task time: "
            , input
                [ Attr.type_ "number"
                , Attr.placeholder "Task time in minutes "
                , Attr.value <| String.fromInt model.taskTime
                , onInput UpdateTaskTime
                ]
                []
            , text "Short break time: "
            , input
                [ Attr.type_ "number"
                , Attr.placeholder "Short break time in minutes"
                , Attr.value <| String.fromInt model.shortBreakTime
                , onInput UpdateShortTime
                ]
                []
            , text "Long break time: "
            , input
                [ Attr.type_ "number"
                , Attr.placeholder "Long brak time in minutes"
                , Attr.value <| String.fromInt model.longBreakTime
                , onInput UpdateLongTime
                ]
                []
            ]
        ]


printTime : Int -> String
printTime t =
    if t < 10 then
        "0" ++ String.fromInt t

    else
        String.fromInt t
