import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )

import MD5
import List exposing (foldl, indexedMap)
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
    , pads: List ( Int, String )
    , maybePads: List ( Int, String, Char ) -- sequences we're searching for in the next 1000 
    , input: String
    }


init: ( Model, Cmd Msg )
init =
    --( Model 64 0 1000 [] [] "abc" -- test
    ( Model 64 0 1000 [] [] "cuanljph"
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
                    ( model |> process
                    , Task.perform ( \_ -> NextHash ) ( Task.succeed () )
                    --, Cmd.none
                    )

                False ->
                    ( model, Cmd.none )


process: Model -> Model
process model =
    let
        hash =
            ( model.input ++ ( model.index |> toString ) )
                |> MD5.hex
                |> get2016Hash 2016 -- Part 2

        threeFive =
            ( hash ++ ":" )
                |> searchSequences '.' "" []
                |> List.filter ( \s -> String.length s >= 3 )

        ( three, five ) =
            threeFive |> List.partition ( \s -> String.length s < 5 )

        newPads =
            five |> List.concatMap
                ( \seq ->
                    case ( String.uncons seq ) of
                        Just ( char, _ ) ->
                            model.maybePads
                                |> List.filterMap ( \( i, p, c ) ->
                                    if ( c == char ) then
                                        Just ( i, p )
                                    else
                                        Nothing
                                )
                        Nothing ->
                            []
                )

        newMaybePads =
            case threeFive of
                seq :: _ ->
                     case seq |> String.uncons of
                        Just ( c, _ ) ->
                            [ ( model.index, hash, c ) ]
                        Nothing ->
                            []
                [] ->
                    []

        filteredMaybePads =
            model.maybePads
                |> List.filter
                    ( \( i, p, c ) ->
                        ( model.index - i <= model.indexDelta ) &&  ( List.member ( i, p ) newPads |> not )
                    )
    in
        { model 
        | index = model.index + 1
        , pads = model.pads ++ newPads
        , maybePads = filteredMaybePads ++ newMaybePads
        }


get2016Hash: Int -> String -> String
get2016Hash current input =
    case current > 0 of
        True ->
            get2016Hash ( current - 1 ) ( input |> MD5.hex )
        False ->
            input


searchSequences: Char -> String -> List String -> String -> List String
searchSequences previous sequence foundSequences input =
    case ( String.uncons input ) of
        Just ( c, nextInput ) ->
            let
                ( newSequence, newFoundSequences ) =
                    case c == previous of
                        True ->
                            ( sequence ++ ( c |> String.fromChar ), foundSequences )

                        False ->
                            case ( String.length sequence > 1 ) of
                                True ->
                                    ( c |> String.fromChar, foundSequences ++ [ sequence ] )
                                False ->
                                    ( c |> String.fromChar, foundSequences )
            in
                searchSequences c newSequence newFoundSequences nextInput

        Nothing ->
            foundSequences


-- VIEW

view: Model -> Html Msg
view model =
    let
        sorted =
            model.pads 
                |> List.sortWith
                    ( \( ia, ha ) ( ib, hb ) ->
                        case compare ia ib of
                            LT -> LT
                            EQ -> EQ
                            GT -> GT
                    )
                |> List.take model.required

    in
        div [ style [ ( "padding", "20px" ) ] ]
            [ div [] [ text ( "One time pads found: " ++ ( sorted |> List.length |> toString ) ) ]
            , div []
                [ ul []
                    ( List.map ( \( i, p ) -> li [] [ text ( toString ( i, p ) ) ] ) sorted
                    )
                ]
            , div [] [ button [ onClick NextHash ] [ text "Next hash" ] ]
            ]
