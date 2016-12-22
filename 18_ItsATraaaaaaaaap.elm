import Html exposing (..)
import Html.Events exposing ( onClick )

import Task
import List

import Debug exposing (log)


main: Program Never Model Msg
main = 
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { maxRow: Int
    , row: String
    , rowNum: Int
    , safeCount: Int
    }


init: ( Model, Cmd Msg )
init =
    -- ( Model 40 input 1 ( input |> countSafeTiles ) -- Part 1
    ( Model 400000 input 1 ( input |> countSafeTiles ) -- Part 2
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- UPDATE

type Msg 
    = NoOp
    | NextStep


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NextStep ->
            case model.maxRow > model.rowNum of
                True ->
                    let
                        newRow =
                            model.row |> getNextRow
                    in
                        ( { model
                            | row = newRow
                            , rowNum = model.rowNum + 1
                            , safeCount = model.safeCount + ( newRow |> countSafeTiles )
                            }
                        , Task.perform ( \_ -> NextStep ) ( Task.succeed () )
                        )

                False ->
                    ( model, Cmd.none )


getNextRow: String -> String
getNextRow input =
    let
        modifiedInput =
            "." ++ input ++ "."
    in
        input
            |> String.length
            |> List.range 1
            |> List.foldl
                ( \i c ->
                    c ++ ( modifiedInput |> getNextTile ( i - 1 ) )
                ) ""


getNextTile: Int -> String -> String
getNextTile idx input =
    let
        test =
            input |> String.dropLeft idx |> String.left 3

    in
        case test of
            "^^." -> "^"
            ".^^" -> "^"
            "^.." -> "^"
            "..^" -> "^"
            _ -> "."


countSafeTiles: String -> Int
countSafeTiles row =
    row |> String.foldl
        ( \c t ->
            case c == '.' of
                True ->
                    t + 1
                False ->
                    t
        ) 0


-- VIEW

view: Model -> Html Msg
view model =
        div []
            [ div [] [ text "Watch your step! " ]
            , div [] [ text ( "Current row: " ++ ( model.rowNum |> toString ) ) ]
            , div [] [ text ( "Safe tiles count: " ++ ( model.safeCount |> toString ) ) ]
            , div [] [ button [ onClick NextStep ] [ text "Next step" ] ]
            ]


-- INPUT
input: String
input =
    "...^^^^^..^...^...^^^^^^...^.^^^.^.^.^^.^^^.....^.^^^...^^^^^^.....^.^^...^^^^^...^.^^^.^^......^^^^"
