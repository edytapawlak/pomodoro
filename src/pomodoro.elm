module Main exposing (Msg(..), Timer, init, main, step, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
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
    , focusTime :  Int
    }


init : () -> ( Timer, Cmd Msg )
init _ =
    ( Timer 0 0 Nothing 0 False 0
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


update : Msg -> Timer -> ( Timer, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model
                | minutes = 0
                , seconds = 5
                , sub = True
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
            ( { model | minutes = 0, seconds = 7, state = Just LongB, succesed = model.succesed - 3 }
            , Cmd.none
            )

        ShortBreak ->
            ( { model | minutes = 0, seconds = 3, state = Just ShortB, succesed = model.succesed - 1 }
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
                succesTimer 25 <| resetTimer timer
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


succesTimer :  Int -> Timer -> Timer
succesTimer n t =
    { t
        | succesed = t.succesed + 1
        , focusTime = t.focusTime + n
    }



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
            , button [ onClick Start ] [ text "Start" ]
            , button [ onClick Stop ] [ text "Stop" ]
            , button [ onClick Continue ] [ text "Continue" ]
            ]
        , div
            []
            [ button [ onClick ShortBreak, Attr.disabled (not (model.succesed >= 1)) ] [ text "Short break " ]
            , button [ onClick LongBreak, Attr.disabled (not (model.succesed >= 3)) ] [ text "Long break" ]
            , h2 [] [ text <| "Your points: " ++ (String.fromInt model.succesed)
            , h2 [] [ text <| "You staied focused for " ++ (String.fromInt model.focusTime) ++ " minutes"]
            ]
            ]
        ]


printTime : Int -> String
printTime t =
    if t < 10 then
        "0" ++ String.fromInt t

    else
        String.fromInt t
