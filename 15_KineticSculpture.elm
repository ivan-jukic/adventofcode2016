import Html exposing (..)
import Html.Events exposing ( onClick )

import List exposing (foldl, indexedMap)
import Task


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
    { discs: List ( Int, Int )
    , time: Int
    , final: Bool
    }


init: ( Model, Cmd Msg )
init =
    ( Model input 0 False
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- UPDATE

type Msg 
    = NoOp
    | NextTick


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NextTick ->
            case model.final of
                False ->
                    let
                        newTime =
                            model.time + 1

                        newDiscs =
                            model.discs |> List.map
                                ( \( pos, posMax ) ->
                                    ( ( pos + 1 ) % posMax, posMax )
                                )

                        newFinal =
                            case newDiscs of
                                ( p, m ) :: otherDiscs ->
                                    otherDiscs |> isSolution p 1 True
                                _ ->
                                    False
                    in
                        ( { model
                            | discs = newDiscs
                            , time = newTime
                            , final = newFinal
                            }
                        , Task.perform ( \_ -> NextTick ) ( Task.succeed () )
                        )

                True ->
                    ( model, Cmd.none )


isSolution: Int -> Int -> Bool -> List ( Int, Int ) -> Bool
isSolution firstPos time eqFirstPos posList =
    case eqFirstPos of
        True ->
            case posList of
                ( pos, maxPos ) :: otherPosList ->
                    let
                        nextPos =
                            ( pos + time ) % maxPos

                        isEqFirstPos =
                            nextPos == firstPos
                    in
                        isSolution firstPos ( time + 1 ) isEqFirstPos otherPosList
                [] ->
                    eqFirstPos
        False ->
            eqFirstPos


-- VIEW

view: Model -> Html Msg
view model =
    div []
        [ div [] [ text ( "discs time: " ++ ( ( model.time - 1 ) |> toString ) ) ]
        , div [] [ button [ onClick NextTick ] [ text "Next tick" ] ]
        ]


-- INPUT
input: List ( Int, Int )
input =
    --[ ( 4, 5 ), ( 1, 2 ) ] -- disc #, current pos, max pos
    [ ( 11, 13 ), ( 0, 5 ), ( 11, 17 ), ( 0, 3 ), ( 2, 7 ), ( 17, 19 ), ( 0, 11 ) ]
