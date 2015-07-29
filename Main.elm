module Main where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Signal, Address, Message)
import String exposing (toInt)

main =
    StartApp.start
        { model = init
        , update = update
        , view = view
        }


type alias Model =
    { year : Maybe Int
    , month : Maybe Int
    , day : Maybe Int
    , column : Maybe Int -- reduce to actual permissible set
    }


init : Model
init =
    { year = Nothing
    , month = Nothing
    , day = Nothing
    , column = Nothing
    }


type Action
    = Noop
    | SetYear (Maybe Int)
    | SetMonth (Maybe Int)
    | SetDay (Maybe Int)
    | SetColumn (Maybe Int)


update : Action -> Model -> Model
update action model =
    case action of
        Noop ->
            model

        SetYear maybeYear ->
            { model |
                year <- maybeYear
            }

        SetMonth maybeMonth ->
            { model |
                month <- maybeMonth
            }

        SetDay maybeDay ->
            { model |
                day <- maybeDay
            }

        SetColumn maybeColumn ->
            { model |
                column <- maybeColumn
            }

maybeIntToString : Maybe Int -> String
maybeIntToString maybe_num =
    case maybe_num of
        Nothing -> ""

        Just a -> toString a

toMaybeInt : String -> Maybe Int
toMaybeInt str =
    case toInt str of
        Err _ ->
            Nothing

        Ok n ->
            Just n

doIfInt: (Maybe Int -> Action) -> Address Action -> String -> Message
doIfInt actionMaker address str =
    case str of
        "" -> Signal.message address (actionMaker Nothing)

        _ -> case toInt str of
            Err _ ->
                Signal.message address Noop

            Ok n ->
                Signal.message address (actionMaker (Just n))

yearSetter: Address Action -> String -> Message
yearSetter address =
    doIfInt (SetYear) address

view : Address Action -> Model -> Html
view address model =
    div
        []
        [ input
            [ id "model-year"
            , placeholder "Year"
            , autofocus True
            , value (maybeIntToString model.year)
            , name "year"
            , on "input" targetValue (yearSetter address)
            ]
            []
        , input
            [ id "model-month"
            , placeholder "Month"
            , autofocus True
            , value (maybeIntToString model.month)
            , name "month"
            , on "input" targetValue (Signal.message address << SetMonth << toMaybeInt)
            ]
            []
        , input
            [ id "model-day"
            , placeholder "Day"
            , autofocus True
            , value (maybeIntToString model.day)
            , name "day"
            ]
            []
        , input
            [ id "model-column"
            , placeholder "Column"
            , autofocus True
            , value (maybeIntToString model.column)
            , name "column"
            ]
            []
        , text (toString model.year)
        ]
