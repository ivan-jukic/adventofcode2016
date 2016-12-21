import Html exposing (..)
import Html.Events exposing ( onClick )

import Task

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
    { step: Int
    , data: String
    , checksum: String
    , targetLen: Int
    }


init: ( Model, Cmd Msg )
init =
    --( Model 0 input "" 272 -- Part 1
    ( Model 0 input "" 35651584 -- Part 2
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- UPDATE

type Msg 
    = NoOp
    | Iterate


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Iterate ->
            case ( String.length model.data == model.targetLen ) of
                True ->
                    let
                        d = log "Get checksum" 1
                    in
                        ( { model | checksum = model.data |> getDataChecksum2 "" }
                        , Cmd.none
                        )

                False ->
                    let
                        newData =
                            model.data
                                |> getNewData
                                |> String.left model.targetLen
                    in
                        ( { model
                            | step = model.step + 1
                            , data = newData
                            }
                        , Task.perform ( \_ -> Iterate ) ( Task.succeed () )
                        )


getNewData: String -> String
getNewData a =
    let
        b = a
            |> String.reverse
            |> String.map ( \c -> if c == '1' then '0' else '1' )
    in
        a ++ "0" ++ b


getDataChecksum2: String -> String -> String
getDataChecksum2 checksum data =
    case ( data |> String.length ) == 0 of
        True ->
            case ( checksum |> String.length ) % 2 == 0 of
                True ->
                    getDataChecksum2 "" checksum
                False ->
                    checksum

        False ->
            let
                newChecksumChar =
                    case ( data |> String.left 2 ) of
                        "11" -> "1"
                        "00" -> "1"
                        _ -> "0"
            in
                getDataChecksum2 ( checksum ++ newChecksumChar ) ( data |> String.dropLeft 2 )


-- VIEW

view: Model -> Html Msg
view model =
    div []
        [ div [] [ text ( "Step: " ++ ( toString model.step ) ) ]
        , div [] [ text ( "Data length: " ++ ( model.data |> String.length |> toString ) ) ]
        , div [] [ text ( "Checksum: " ++ model.checksum ) ]
        , div [] [ button [ onClick Iterate ] [ text "Iterate" ] ]
        ]


-- INPUT
input: String
input = 
    "10011111011011001"
