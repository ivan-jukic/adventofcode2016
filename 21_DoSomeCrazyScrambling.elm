import Html exposing (..)
import Html.Events exposing ( onClick )

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
    { pass: String
    , cmds: List String
    }


init: ( Model, Cmd Msg )
init =
    ( Model startingCode ( input |> String.lines )
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- UPDATE

type Msg 
    = NoOp
    | NextInstruction


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NextInstruction ->
            case model.cmds of
                cmd :: other ->
                    ( { model | pass = ( model.pass |> processCmd cmd ), cmds = other }, Cmd.none )
                [] ->
                    ( model, Cmd.none )
            

processCmd: String -> String -> String
processCmd cmd pass =
    case ( cmd |> String.words ) of
        "swap"::"position"::x::"with"::"position"::y::[] ->
            pass |> swapPosition ( x |> strToInt ) ( y |> strToInt )
        "swap"::"letter"::x::"with"::"letter"::y::[] ->
            pass |> swapLetter x y
        "rotate"::dir::x::"step"::[] ->
            pass |> rotatePass dir ( x |> strToInt )
        "rotate"::dir::x::"steps"::[] ->
            pass |> rotatePass dir ( x |> strToInt )
        "rotate"::"based"::"on"::"position"::"of"::"letter"::x::[] ->
            pass |> rotateBasedOnLetter x
        "reverse"::"positions"::x::"through"::y::[] ->
            pass |> reverseSubPass ( x |> strToInt ) ( y |> strToInt )
        "move"::"position"::x::"to"::"position"::y::[] ->
            pass |> moveTo ( x |> strToInt ) ( y |> strToInt )
        _ ->
            pass


swapPosition: Int -> Int -> String -> String
swapPosition x y pass =
    let
        ( lx, ly ) =
            ( pass |> String.dropLeft x |> String.left 1
            , pass |> String.dropLeft y |> String.left 1
            )

        newPass =
            ( pass |> String.left y ) ++ lx ++ ( pass |> String.dropLeft ( y + 1 ) )

        d = log "swap position" ( x, y, lx, ly, pass, newPass )
    in
        pass


swapLetter: String -> String -> String -> String
swapLetter x y pass =
    let
        d = log "swap letter" ( x, y )
    in
        pass


rotatePass: String -> Int -> String -> String
rotatePass dir steps pass =
    let
        d = log "rotate" ( dir, steps )
    in
        pass


rotateBasedOnLetter: String -> String -> String
rotateBasedOnLetter letter pass =
    let
        d = log "rotate based on letter" letter
    in
        pass


reverseSubPass: Int -> Int -> String -> String
reverseSubPass x y pass =
    let
        d = log "reverse from" ( x, y )
    in
        pass


moveTo: Int -> Int -> String -> String
moveTo x y pass =
    let
        d = log "move from position to position" ( x, y )
    in
        pass


strToInt: String -> Int
strToInt a =
    a |> String.toInt |> Result.withDefault 0


-- VIEW

view: Model -> Html Msg
view model =
        div []
            [ div [] [ text ( "Current scrambled pass: " ++ ( model.pass ) ) ]
            , div [] [ button [ onClick NextInstruction ] [ text "Next instruction" ] ]
            ]


-- INPUT
startingCode: String
startingCode =
    "abcde"
    -- "abcdefgh"


input: String
input =
    """swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d
"""
{-- }
    """rotate left 2 steps
rotate right 0 steps
rotate based on position of letter a
rotate based on position of letter f
swap letter g with letter b
rotate left 4 steps
swap letter e with letter f
reverse positions 1 through 6
swap letter b with letter d
swap letter b with letter c
move position 7 to position 5
rotate based on position of letter h
swap position 6 with position 5
reverse positions 2 through 7
move position 5 to position 0
rotate based on position of letter e
rotate based on position of letter c
rotate right 4 steps
reverse positions 3 through 7
rotate left 4 steps
rotate based on position of letter f
rotate left 3 steps
swap letter d with letter a
swap position 0 with position 1
rotate based on position of letter a
move position 3 to position 6
swap letter e with letter g
move position 6 to position 2
reverse positions 1 through 2
rotate right 1 step
reverse positions 0 through 6
swap letter e with letter h
swap letter f with letter a
rotate based on position of letter a
swap position 7 with position 4
reverse positions 2 through 5
swap position 1 with position 2
rotate right 0 steps
reverse positions 5 through 7
rotate based on position of letter a
swap letter f with letter h
swap letter a with letter f
rotate right 4 steps
move position 7 to position 5
rotate based on position of letter a
reverse positions 0 through 6
swap letter g with letter c
reverse positions 5 through 6
reverse positions 3 through 5
reverse positions 4 through 6
swap position 3 with position 4
move position 4 to position 2
reverse positions 3 through 4
rotate left 0 steps
reverse positions 3 through 6
swap position 6 with position 7
reverse positions 2 through 5
swap position 2 with position 0
reverse positions 0 through 3
reverse positions 3 through 5
rotate based on position of letter d
move position 1 to position 2
rotate based on position of letter c
swap letter e with letter a
move position 4 to position 1
reverse positions 5 through 7
rotate left 1 step
rotate based on position of letter h
reverse positions 1 through 7
rotate based on position of letter f
move position 1 to position 5
reverse positions 1 through 4
rotate based on position of letter a
swap letter b with letter c
rotate based on position of letter g
swap letter a with letter g
swap position 1 with position 0
rotate right 2 steps
rotate based on position of letter f
swap position 5 with position 4
move position 1 to position 0
swap letter f with letter b
swap letter f with letter h
move position 1 to position 7
swap letter c with letter b
reverse positions 5 through 7
rotate left 6 steps
swap letter d with letter b
rotate left 3 steps
swap position 1 with position 4
rotate based on position of letter a
rotate based on position of letter a
swap letter b with letter c
swap letter e with letter f
reverse positions 4 through 7
rotate right 0 steps
reverse positions 2 through 3
rotate based on position of letter a
reverse positions 1 through 4
rotate right 1 step"""
--}