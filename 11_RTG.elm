import Html exposing (..)
import Html.Events exposing ( onClick )

import Maybe exposing ( withDefault )
import Dict exposing ( Dict )
import Task
import Bitwise


main: Program Never Model Msg
main = 
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- DATA

maxFloor: Int
maxFloor =
    4

{--
 -- PM - promethium
 -- RU - ruthenium
 -- PU - plutonium
 -- SR - strontium
 -- TM - thulium
 --}
type Item =
    PMG | PMM | RUG | RUM | PUG | PUM | SRG | SRM | TMG | TMM


gensOnly: List Item
gensOnly =
    [ PMG, RUG, PUG, SRG, TMG ]


chipsOnly: List Item
chipsOnly =
    [ PMM, RUM, PUM, SRM, TMM ]


itemWeights: Item -> Int
itemWeights item =
    case item of
        PMG -> 1
        PMM -> 2
        RUG -> 4
        RUM -> 8
        PUG -> 16
        PUM -> 32
        SRG -> 64
        SRM -> 128
        TMG -> 256
        TMM -> 512


type alias State =
    { depth: Int
    , floor: Int
    , final: Bool
    , locations: Dict Int ( List Item )
    , encoded: String
    }


{--
    F4 . ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
    F3 . ___ ___ ___ ___ ___ ___ PMG PMM RUG RUM
    F2 . ___ ___ ___ PUM ___ SRM ___ ___ ___ ___
    F1 E TMG TMM PUG ___ SRG ___ ___ ___ ___ ___
--}

initialLocations: Dict Int ( List Item )
initialLocations =
    Dict.fromList <|
        {-- }
        [ ( 4, [] )
        , ( 3, [ PUG ] )
        , ( 2, [ TMG ] )
        , ( 1, [ TMM, PUM ] )
        ]
        --}
        {--}
        [ ( 4, [] )
        , ( 3, [ PMG, PMM, RUG, RUM ] )
        , ( 2, [ PUM, SRM ] )
        , ( 1, [ TMG, TMM, PUG, SRG ] )
        ]
        --}

initialState: State
initialState =
    { depth = 0
    , floor = 1
    , final = False
    , locations = initialLocations
    , encoded = encodeState 1 initialLocations
    }


-- MODEL

type alias Model =
    { step: Int
    , states: List State
    , finished: List State
    , processed: List String
    }


init: ( Model, Cmd Msg )
init =
    ( Model 0 [ initialState ] [] [], Cmd.none )


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
            let
                ( newStates, processed ) =
                    model.states
                        |> List.foldl
                            ( \s (ns, pr) ->
                                let
                                    ( nns, npr ) =
                                         s |> getNextPossibleStates pr
                                in
                                    ( ns ++ nns, npr )
                            ) ( [], model.processed )

                ( finished, nonFinished ) =
                    newStates
                        |> List.partition ( \s -> s.final )


                action =
                    case ( List.length finished ) > 0 of
                        True ->
                            Cmd.none
                        False ->
                            --Task.perform ( \_ -> NextStep ) ( Task.succeed () )
                            Cmd.none

                c = Debug.log "Step" ( model.step, List.length model.states, List.length model.processed )
                    
                d = 
                    case finished of
                        a :: _ ->
                            Debug.log "finished" ( model.step + 1, List.length finished, Dict.toList a.locations )
                        [] ->
                            ( 0, 0, [] )

            in
                ( { model
                    | step = model.step + 1
                    , states = nonFinished
                    , finished = model.finished ++ finished
                    , processed = processed
                    }
                , action )


getNextPossibleStates: List String -> State -> ( List State, List String )
getNextPossibleStates processed state =
    case ( Dict.get state.floor state.locations ) of
        Just items ->
            let
                ( newUpStates, upProcessed ) =
                    state |> takeToFloor ( state.floor + 1 ) items processed

                ( newDownStates, downProcessed ) =
                    state |> takeToFloor ( state.floor - 1 ) items upProcessed
                    
                newStates =
                    ( newUpStates ++ newDownStates ) |> List.map
                        ( \s -> { s | final = ( isFinalState s ) } )
            in
                ( newStates, downProcessed )
        Nothing ->
            ( [], processed )


takeToFloor: Int -> List Item -> List String -> State -> ( List State, List String )
takeToFloor newFloor items processed state =
    case ( Dict.get newFloor state.locations ) of
        Just nextItems ->
            let
                ( newStates, newProcessed ) =
                    items
                        |> getCombinations 
                        |> List.foldl
                            ( \comb ( s, proc ) ->
                                let
                                    ( currentFloor, nextFloor ) =
                                        ( items |> List.filter ( \i -> not ( List.member i comb ) )
                                        , nextItems ++ comb
                                        )
                                in
                                    case (currentFloor |> isValidFloor, nextFloor |> isValidFloor ) of
                                        ( True, True ) ->
                                            let
                                                newLocations =
                                                    state.locations
                                                        |> Dict.insert state.floor currentFloor
                                                        |> Dict.insert newFloor nextFloor
                                                        
                                                newState =
                                                    { depth = state.depth + 1
                                                    , floor = newFloor
                                                    , final = False
                                                    , locations = newLocations
                                                    , encoded = encodeState newFloor newLocations
                                                    }

                                                isProcessed =
                                                    List.member newState.encoded proc

                                                --d = Debug.log "encoded" ( isProcessed, newState.encoded, List.length proc )

                                                {-- }
                                                d =
                                                    if wasNotProcessed then
                                                        ( Debug.log "curr" ( state.floor, Dict.toList state.locations )
                                                        , Debug.log "next" ( newFloor , Dict.toList newLocations )
                                                        )
                                                    else
                                                        ( ( 0, [] )
                                                        , ( 0, [] )
                                                        )
                                                --}
                                            in
                                                case isProcessed of
                                                    True ->
                                                        ( s, proc )
                                                    False ->
                                                        ( s ++ [ newState ], proc ++ [ newState.encoded ] )
                                        ( _, _ ) ->
                                            ( s, proc )
                            ) ( [], processed )
            in
                ( newStates, newProcessed )
        Nothing ->
            ( [], processed )


getCombinations: List Item -> List ( List Item )
getCombinations items =
    items |> List.foldl
        ( \i c ->
            items |> List.foldl
                ( \ii cc ->
                    case i == ii || ( cc |> List.member [ i, ii ] ) || ( cc |> List.member [ ii, i ] ) of
                        True ->
                            cc
                        False ->
                            cc ++ [ [ i, ii ] ]
                ) ( c ++ [ [ i ] ] )
        ) []


isValidFloor: List Item -> Bool
isValidFloor floorItems =
    case ( floorItems |> hasGenerators ) of
        True ->
            floorItems |> List.foldl
                ( \i b ->
                    case b of
                        True ->
                            case i of
                                PMM -> floorItems |> floorHasGenerator PMG
                                RUM -> floorItems |> floorHasGenerator RUG
                                PUM -> floorItems |> floorHasGenerator PUG
                                SRM -> floorItems |> floorHasGenerator SRG
                                TMM -> floorItems |> floorHasGenerator TMG
                                _ -> b
                        False ->
                            b
                ) True
        False ->
            True


encodeState: Int -> Dict Int ( List Item ) -> String
encodeState f dict =
        ( toString f ) ++ ">" ++
            ( Dict.toList dict
                |> List.map
                    ( \( i, items ) ->
                        let
                            factor =
                                List.foldl ( \item c -> Bitwise.or c ( itemWeights item ) ) 0 items
                        in
                            ( toString i ) ++ ":" ++ ( toString factor )
                    )
                |> String.join ";"
            )



hasGenerators: List Item -> Bool
hasGenerators items =
    items |> List.any
        ( \i -> List.member i gensOnly )


floorHasGenerator: Item -> List Item -> Bool
floorHasGenerator gen items =
    List.member gen items


isFinalState: State -> Bool
isFinalState state =
    state.locations |> Dict.foldl
        ( \key val isFinal ->
            case isFinal of
                True ->
                    ( List.length val == 0 && key /= maxFloor ) || key == maxFloor
                False ->
                    isFinal
        ) True


-- VIEW

view: Model -> Html Msg
view model =
    div []
        [ div [] [ text ( "generators and chips step: " ++ ( toString model.step ) ) ]
        , div []
            [ button [ onClick NextStep ] [ text "Next step" ]
            ]
        ]
