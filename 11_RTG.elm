import Html exposing (..)
import Html.Events exposing ( onClick )

import Maybe exposing ( withDefault )
import Dict exposing ( Dict )
import Task
import Bitwise
import Trampoline exposing (Trampoline)


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
                    Trampoline.evaluate <| ( getNextPossibleStates model.processed [] model.states )

                ( finished, nonFinished ) =
                    newStates |> List.partition ( \s -> s.final )

                action =
                    case ( List.length finished ) > 0 of
                        True ->
                            Cmd.none
                        False ->
                            Task.perform ( \_ -> NextStep ) ( Task.succeed () )
                            --Cmd.none

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


getNextPossibleStates: List String -> List State -> List State -> Trampoline ( List State, List String )
getNextPossibleStates processed newStates states =
    case states of
        [] ->
            Trampoline.done ( newStates, processed )

        s :: otherStates ->
            case ( Dict.get s.floor s.locations ) of

                Nothing ->
                    Trampoline.done ( newStates, processed )

                Just currentFloorItems ->
                    let
                        combinations =
                            Trampoline.evaluate <| ( getCombinations currentFloorItems currentFloorItems [] )

                        ( newUpStates, upProcessed ) =
                            Trampoline.evaluate <| ( s |> takeToFloor ( s.floor + 1 ) currentFloorItems combinations processed [] )
                        
                        ( newDownStates, newProcessed ) =
                            Trampoline.evaluate <| ( s |> takeToFloor ( s.floor - 1 ) currentFloorItems combinations upProcessed [] )

                        newUpDownStates =
                            ( newUpStates ++ newDownStates ) |> List.map
                                ( \s -> { s | final = ( Trampoline.evaluate <| ( isFinalState 1 True s ) ) } )
                    in
                        Trampoline.jump ( \_ -> getNextPossibleStates newProcessed ( newStates ++ newUpDownStates ) otherStates )


takeToFloor: Int -> List Item -> List ( List Item ) -> List String -> List State -> State -> Trampoline ( List State, List String )
takeToFloor nf floorItems combinations processed newStates s =
    case ( Dict.get nf s.locations ) of
        Nothing ->
            Trampoline.done ( newStates, processed )

        Just nextFloorItems ->
            case combinations of
                [] ->
                    Trampoline.done ( newStates, processed )
                comb :: otherComb ->
                    let
                        ( cfItems, nfItems ) =
                            ( floorItems |> List.filter ( \i -> not ( List.member i comb ) )
                            , nextFloorItems ++ comb
                            )

                        ( ns, newProcessed ) =
                            addNewState s.floor cfItems nf nfItems s.depth processed s.locations
                    in
                        Trampoline.jump ( \_ -> takeToFloor nf floorItems otherComb newProcessed ( newStates ++ ns ) s )


addNewState: Int -> List Item -> Int -> List Item -> Int -> List String -> Dict Int ( List Item ) -> ( List State, List String )
addNewState cf currentFloor nf nextFloor depth processed currentLocations =
    case ( currentFloor |> isValidFloor, nextFloor |> isValidFloor ) of
        ( True, True ) ->
            let
                newLocations =
                    currentLocations
                        |> Dict.insert cf currentFloor
                        |> Dict.insert nf nextFloor

                newState =
                    { depth = depth + 1
                    , floor = nf
                    , final = False
                    , locations = newLocations
                    , encoded = encodeState nf newLocations
                    }

                isProcessed =
                    List.member newState.encoded processed

                -- d = Debug.log "encoded" ( isProcessed, ( nf, nextFloor ), List.length processed )
            in
                case isProcessed of
                    True ->
                        ( [], processed )
                    False ->
                        ( [ newState ], processed ++ [ newState.encoded ] )
        ( _, _ ) ->
            ( [], processed )


getCombinations: List Item -> List Item -> List ( List Item ) -> Trampoline ( List ( List Item ) )
getCombinations floorItems fullItems combos =
    case floorItems of
        [] ->
           Trampoline.done combos
        item :: otherItems ->
            let
                newCombos = 
                    [ [ item ] ] ++ 
                    ( fullItems
                        |> List.filter
                            ( \i -> 
                                i /= item && 
                                not ( List.member [ i, item ] combos || List.member [ item, i ] combos )
                            )
                        |> List.map ( \i -> [ i, item ] )
                    )
            in
                case ( List.length newCombos ) > 0 of
                    False ->
                        Trampoline.done combos
                    True ->
                        Trampoline.jump ( \_ -> getCombinations otherItems fullItems ( combos ++ newCombos ) )


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


isFinalState: Int -> Bool -> State -> Trampoline Bool
isFinalState cf final s =
    case final of 
        False ->
            Trampoline.done final
        True ->
            case cf < maxFloor of
                True ->
                    case ( Dict.get cf s.locations ) of
                        Just items ->
                            Trampoline.jump ( \_ -> isFinalState ( cf + 1 ) ( List.length items == 0 ) s )
                        Nothing ->
                            Trampoline.done final
                False ->
                    Trampoline.done final


-- VIEW

view: Model -> Html Msg
view model =
    div []
        [ div [] [ text ( "generators and chips step: " ++ ( toString model.step ) ) ]
        , div []
            [ button [ onClick NextStep ] [ text "Next step" ]
            ]
        ]
