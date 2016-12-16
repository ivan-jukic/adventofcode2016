import Html exposing (..)
import Html.Events exposing ( onClick )

import MD5
import List exposing (foldl)
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
    { required: Int
    , index: Int
    , indexDelta: Int
    , pads: List String
    , sequence: List ( Int, String, String ) -- sequences we're searching for in the next 1000 
    , input: String
    }


init: ( Model, Cmd Msg )
init =
    ( Model 64 0 1000 [] [] "abc" -- test
    --( Model 64 0 [] [] "cuanljph" -- Part 1
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- UPDATE

type Msg 
    = NoOp
    | NextHash


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NextHash ->
            case ( List.length model.pads < model.required) of
                True ->
                    model |> process

                False ->
                    ( model, Cmd.none )


process: Model -> ( Model, Cmd Msg )
process model =
    let
        hash =
            ( model.input ++ ( model.index |> toString ) ) |> MD5.hex

        d = log "hash" ( model.index, hash )
    in
        ( { model | index = model.index + 1 }, Cmd.none )


hasSequences: String -> ( Bool, Bool )
hasSequences src =
    ( False, False )


-- VIEW

view: Model -> Html Msg
view model =
    div []
        [ div [] [ text "One time pads!!" ]
        , div [] [ button [ onClick NextHash ] [ text "Next hash" ] ]
        ]
