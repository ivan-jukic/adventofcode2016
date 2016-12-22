import Html exposing (..)
import Html.Events exposing ( onClick )

import Task
import Dict exposing (Dict)
import Array exposing (Array)

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


maxElves: Int
maxElves =
    --5 -- Test
    3012210 -- Part 1


type alias Model =
    { num: Int
    , elves: Array Int
    , final: Bool
    }


init: ( Model, Cmd Msg )
init =
    ( Model 1 Array.empty False
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- UPDATE

type Msg 
    = NoOp
    | Calculate


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Calculate ->
            case model.final of
                False ->
                    let
                        elves =
                            Array.initialize maxElves ( \n -> n + 1 )
                                |> takeOpposingGifts 0
                    in
                        ( { model | elves = elves } , Cmd.none )
                True ->
                    ( model, Cmd.none )


takeOpposingGifts: Int -> Array Int -> Array Int
takeOpposingGifts idx elves =
    let
        len =
            elves |> Array.length
    in
        case len == 1 of
            True ->
                elves

            False ->
                let
                    removeElfOnDistance =
                        case len % 2 == 0 of
                            True ->
                                len // 2
                            False ->
                                floor ( ( len |> toFloat ) / 2 )

                    removeIdx =
                        ( idx + removeElfOnDistance ) % len

                    newElves =
                        Array.append
                            ( elves |> Array.slice 0 removeIdx )
                            ( elves |> Array.slice ( removeIdx + 1 ) len )

                    newIdx =
                        case idx < removeIdx of
                            True ->
                                ( idx + 1 ) % ( len - 1 )
                            False ->
                                idx % ( len - 1 )

                    d = log "new elves" ( idx, removeIdx )
                    -- d = log "new elves" ( idx, elves |> Array.toList, removeIdx, newElves |> Array.toList )
                in
                    newElves |> takeOpposingGifts newIdx


-- VIEW

view: Model -> Html Msg
view model =
        div []
            [ div [] [ text "Elves stealing gifts!" ]
            , div [] [ text ( "Final elf remaining: " ++ ( model.elves |> Array.toList |> toString ) ) ]
            , div [] [ button [ onClick Calculate ] [ text "Calculate" ] ]
            ]
