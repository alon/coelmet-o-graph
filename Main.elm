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

-- ((((  Model ))))

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


modelFilename : Model -> Maybe String
modelFilename model =
    case (model.year, model.month, model.day, model.column) of
        (Just year, Just month, Just day, Just column) ->
            Just "GOOD ONE"

        _ ->
            Nothing


-- (((( Action ))))

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
            , value (maybeIntToString model.year)
            , name "year"
            , on "input" targetValue (yearSetter address)
            ]
            []
        , input
            [ id "model-month"
            , placeholder "Month"
            , value (maybeIntToString model.month)
            , name "month"
            , on "input" targetValue (Signal.message address << SetMonth << toMaybeInt)
            ]
            []
        , input
            [ id "model-day"
            , placeholder "Day"
            , value (maybeIntToString model.day)
            , name "day"
            , on "input" targetValue (Signal.message address << SetDay << toMaybeInt)
            ]
            []
        , input
            [ id "model-column"
            , placeholder "Column"
            , value (maybeIntToString model.column)
            , name "column"
            , on "input" targetValue (Signal.message address << SetColumn << toMaybeInt)
            ]
            []
            {--
        , text "#"
        , text (toString model.year)
        , text "#"
        , text (toString model.month)
        , text "#"
        , text (toString model.day)
        , text "#"
        , text (toString model.column)
        --}
        , text (case modelFilename model of
                Nothing
                    -> ""
                Just filename
                    -> filename)
        ]
