import Html exposing (..)
import Html.Events exposing ( onClick )

import MD5
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

type alias State =
    { path: String
    , x: Int
    , y: Int
    }


initialState: State
initialState =
    State "" 0 0


type alias Model =
    { states: List State
    , dimX: Int
    , dimY: Int
    , step: Int
    , shortest: Maybe State
    , longest: Maybe State
    }


init: ( Model, Cmd Msg )
init =
    ( Model [ initialState ] 4 4 0 Nothing Nothing
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
            case model.states of
                [] ->
                    ( model, Cmd.none )

                states ->
                    let
                        newStates =
                            states |> calculateNewStates input [] model.dimX model.dimY

                        ( finalStates, newNonFinalStates ) =
                            newStates |> List.partition
                                ( \s -> s.x == ( model.dimX - 1) && s.y == ( model.dimY - 1 ) )

                        ( newShortest, newLongest ) =
                            case finalStates of
                                s :: _ ->
                                    ( case model.shortest of
                                        Just short ->
                                            model.shortest
                                        Nothing ->
                                            Just s
                                    , Just s
                                    )
                                [] ->
                                    ( model.shortest, model.longest )

                        longPathLen =
                            case newLongest of
                                Just long ->
                                    long.path |> String.length
                                Nothing ->
                                    0

                        d = log "step" ( model.step, List.length newNonFinalStates, longPathLen )
                    in
                        ( { model 
                            | states = newNonFinalStates
                            , step = model.step + 1
                            , shortest = newShortest
                            , longest = newLongest
                            }
                        , Task.perform ( \_ -> NextStep ) ( Task.succeed () )
                        )


calculateNewStates: String -> List State -> Int -> Int -> List State -> List State
calculateNewStates input newStates dimX dimY states =
    case states of
        s :: restOfStates ->
            let
                code =
                    ( input ++ s.path )
                        |> MD5.hex
                        |> String.left 4

                addStates =
                    code
                        |> String.toList
                        |> List.indexedMap (,)
                        |> List.concatMap
                            ( \( i, c ) ->
                                case ( List.member c [ 'b', 'c', 'd', 'e', 'f' ] ) of
                                    True ->
                                        case i of
                                            0 -> move -1 0 dimX dimY "U" s -- up
                                            1 -> move 1 0 dimX dimY "D" s -- down
                                            2 -> move 0 -1 dimX dimY "L" s -- left
                                            _ -> move 0 1 dimX dimY "R" s -- right
                                    False ->
                                        []
                            )
            in
                calculateNewStates input ( newStates ++ addStates ) dimX dimY restOfStates
        [] ->
            newStates


move: Int -> Int -> Int -> Int -> String -> State -> List State
move dx dy dimX dimY pathAddition state =
    let
        ( nx, ny ) =
            ( state.x + dx
            , state.y + dy
            )
    in
        case nx < dimX && nx > -1 && ny < dimY && ny > -1 of
            True ->
                [ State ( state.path ++ pathAddition ) nx ny ]
            False ->
                []


-- VIEW

view: Model -> Html Msg
view model =
    let
        shortestPath =
            case model.shortest of
                Just short ->
                    short.path
                Nothing ->
                    "--"

        longestPathLength =
            case model.longest of
                Just long ->
                    long.path |> String.length |> toString
                Nothing ->
                    "--"
    in
        div []
            [ div [] [ text ( "Current step: " ++ ( model.step |> toString ) ) ]
            , div [] [ text ( "Shortest path: " ++ shortestPath ) ]
            , div [] [ text ( "Longest path no. steps: " ++ longestPathLength ) ]
            , div [] [ button [ onClick NextStep ] [ text "Next step" ] ]
            ]


-- INPUT
input: String
input =
    "dmypynyp"
