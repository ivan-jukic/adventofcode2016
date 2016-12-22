import Html exposing (..)
import Html.Events exposing ( onClick )

import Task
import List
import Regex exposing ( regex, HowMany(AtMost), split, contains )

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

separator: String
separator =
    "-"


input: Int
input =
    --5 -- Test
    3012210 -- Part 1


type alias Model =
    { num: Int
    , elves: String
    , final: Bool
    }


init: ( Model, Cmd Msg )
init =
    ( Model input ( getElvenList input ) False
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
            case model.final of
                False ->
                    let
                        newElves =
                            model.elves |> takeGifts ""
                    in
                        ( { model
                            | elves = newElves
                            , final = newElves |> isFinal
                            }
                        , Cmd.none
                        )
                True ->
                    ( model, Cmd.none )


takeGifts: String -> String -> String
takeGifts reduced remaining =
    case ( remaining |> matchElves 2 ) of

        idxa :: idxb :: other :: [] ->
            takeGifts ( reduced |> getNewReduced idxa ) other

        idxa :: idxb :: [] ->
            reduced |> getNewReduced idxa

        idx :: [] ->
            case ( reduced |> matchElves 1 ) of
                idxa :: otherReduced :: [] ->
                    otherReduced ++ separator ++ idx
                _ ->
                    idx

        _ ->
            reduced


getNewReduced: String -> String -> String
getNewReduced addition reduced =
    case ( reduced |> String.length ) == 0 of
        True ->
            addition
        False ->
            reduced ++ separator ++ addition


matchElves: Int -> String -> List String
matchElves most input =
    input |> split ( AtMost most ) ( regex separator )


isFinal: String -> Bool
isFinal elves =
    elves |> contains ( regex "-" ) |> not


getElvenList: Int -> String
getElvenList elvesNum =
    elvesNum
        |> List.range 1
        |> List.map ( \v -> v |> toString )
        |> String.join separator


-- VIEW

view: Model -> Html Msg
view model =
        div []
            [ div [] [ text "Elves stealing gifts!" ]
            , div [] [ text ( "Processing: " ++ ( model.elves |> String.length |> toString ) ) ]
            , div []
                ( case model.final of
                    True ->
                        [ text ( "Final elf remaining: " ++ model.elves ) ]
                    False ->
                        []
                )
            , div [] [ button [ onClick NextStep ] [ text "Next step" ] ]
            ]
