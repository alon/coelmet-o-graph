module Main where

import Maybe exposing (withDefault)
import Http exposing (getString)
import Task exposing (Task, andThen)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (..)
import String exposing (toInt)

import Util exposing (formatNumber)
import CSV

main : Signal Html
main =
    Signal.map view model

model : Signal Model
model =
    Signal.foldp update init actions.signal

-- ((((  Model ))))

type alias Model =
    { year : Maybe Int
    , month : Maybe Int
    , day : Maybe Int
    , column : Maybe Int -- reduce to actual permissible set
    , contents : List (List String)
    }


init : Model
init =
    { year = Nothing
    , month = Nothing
    , day = Nothing
    , column = Nothing
    , contents = []
    }


modelFilename : Model -> Maybe String
modelFilename model =
    let
        f2 : Int -> String
        f2 = formatNumber 2 "0"
    in
        case (model.year, model.month, model.day, model.column) of
            (Just year, Just month, Just day, Just column) ->
                Just ("LG" ++ f2 year ++ f2 month ++ f2 day ++ ".CSV")
            _ ->
                Nothing


-- (((( Action ))))

type Action
    = NoOp
    | SetYear (Maybe Int)
    | SetMonth (Maybe Int)
    | SetDay (Maybe Int)
    | SetColumn (Maybe Int)
    | GotResponse String

actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
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

        GotResponse str ->
            { model |
                contents <- CSV.parse str
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
                Signal.message address NoOp

            Ok n ->
                Signal.message address (actionMaker (Just n))


yearSetter: Address Action -> String -> Message
yearSetter address =
    doIfInt (SetYear) address


filename : Signal String
filename =
        Signal.filterMap modelFilename "" model


origin : String
origin =
        -- "http://logger.comet-me.org/tuba/"
        "http://localhost:8000/csv/"

getCometFile : String -> Task Http.Error String
getCometFile filename =
        Http.getString (origin ++ filename)


port requests : Signal (Task Http.Error ())
port requests =
        filename |> Signal.map (\filen -> (getCometFile filen) `andThen` updateModelContents)


updateModelContents : String -> Task Http.Error ()
updateModelContents contents =
        Signal.send actions.address (GotResponse contents)


view : Model -> Html
view model =
    div
        []
        [ input
            [ id "model-year"
            , placeholder "Year"
            , value (maybeIntToString model.year)
            , name "year"
            , on "input" targetValue (yearSetter actions.address)
            ]
            []
        , input
            [ id "model-month"
            , placeholder "Month"
            , value (maybeIntToString model.month)
            , name "month"
            , on "input" targetValue (Signal.message actions.address << SetMonth << toMaybeInt)
            ]
            []
        , input
            [ id "model-day"
            , placeholder "Day"
            , value (maybeIntToString model.day)
            , name "day"
            , on "input" targetValue (Signal.message actions.address << SetDay << toMaybeInt)
            ]
            []
        , input
            [ id "model-column"
            , placeholder "Column"
            , value (maybeIntToString model.column)
            , name "column"
            , on "input" targetValue (Signal.message actions.address << SetColumn << toMaybeInt)
            ]
            []
        , text (case modelFilename model of
                Nothing
                    -> ""
                Just filename
                    -> filename)
        , div [] (List.map (\ll -> div [] (List.map (\l -> div [] [text l]) ll)) model.contents)
        ]
