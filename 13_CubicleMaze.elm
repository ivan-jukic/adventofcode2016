import Html exposing (..)
import Html.Events exposing ( onClick )

import Bitwise exposing (shiftRightBy, and)
import List exposing (foldl)
import Task
import Set exposing (Set)

--import Debug exposing (log)


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
    { favNum: Int
    , step: Int
    , dest: ( Int, Int )
    , positions: List ( Int, Int )
    , visited: List ( Int, Int )
    , distinctPositions: Set ( Int, Int )
    , distinctMaxStep: Int
    , finished: Bool
    }


init: ( Model, Cmd Msg )
init =
    --( Model 10 0 ( 7, 4 ) [ ( 1, 1 ) ] [] Set.empty 5 False, Cmd.none ) -- Test
    {--}
    ( Model
        1364
        0
        ( 31, 39 )
        [ ( 1, 1 ) ]
        []
        Set.empty
        50
        False
    , Cmd.none )
    --}


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- UPDATE

type Msg 
    = NoOp
    | Step


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Step ->
            case model.finished of
                False ->
                    let
                        newDistinctPositions =
                            case model.step <= model.distinctMaxStep of
                                True ->
                                    model.positions
                                        |> foldl ( \(i, j) cs -> Set.insert (i, j) cs ) model.distinctPositions
                                False ->
                                    model.distinctPositions

                        newStep =
                            model.step + 1

                        ( newPositions, newVisited ) =
                            model.positions
                                |> foldl 
                                    ( \( x, y ) ( npos, nvis ) ->
                                        process x y model.favNum nvis npos
                                    ) ( [], model.visited )

                        isFinished =
                            newPositions |> List.member model.dest
                    in
                        ( { model
                            | step = newStep
                            , positions = newPositions
                            , visited = newVisited
                            , finished = isFinished
                            , distinctPositions = newDistinctPositions
                            }
                        , Task.perform ( \_ -> Step ) ( Task.succeed () )
                        --, Cmd.none
                        )

                True ->
                    ( model, Cmd.none )


process: Int -> Int -> Int -> List ( Int, Int ) -> List ( Int, Int ) ->  ( List ( Int, Int ), List ( Int, Int ) )
process x y favNum visited newPositions =
    let
        possibleMoves =
            [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1) ]
                |> List.filter
                    ( \( i, j ) ->
                        isValidPosition i j favNum visited
                    )
    in
        ( newPositions ++ possibleMoves, visited ++ [ ( x, y ) ] )


isValidPosition: Int -> Int -> Int-> List ( Int, Int ) -> Bool
isValidPosition x y favNum visited =
    x > -1 && y > -1 && ( favNum |> isOpenSpace x y ) && ( visited |> List.member ( x, y ) |> not )


isOpenSpace: Int -> Int -> Int -> Bool
isOpenSpace x y num =
    (((x * x + 3 * x + 2 * x * y + y + y * y) + num ) |> numberOfSetBits) % 2 == 0


-- variable-precision SWAR algorithm
numberOfSetBits: Int -> Int
numberOfSetBits num =
    let
        step1 =
            num - ( num |> shiftRightBy 1 |> and 0x55555555 )
        
        step2 =
            ( and step1 0x33333333 ) + ( step1 |> shiftRightBy 2 |> and 0x33333333 )
    in
        ( ( step2 + ( shiftRightBy 4 step2 ) ) |> and 0x0F0F0F0F ) * 0x01010101 |> shiftRightBy 24


-- VIEW

view: Model -> Html Msg
view model =
    div []
        ( case model.finished of
            True ->
                [ div []
                    [ text ( "The shortest path to "
                        ++ ( model.dest |> toString )
                        ++ " is: " ++ ( model.step |> toString ) )
                    ]
                , div []
                    [ text ( "No. of distinct positions up to step "
                        ++ ( model.distinctMaxStep |> toString )
                        ++ " is: "
                        ++ ( Set.size model.distinctPositions |> toString ) )
                    ]
                ]
            False ->
                [ button [ onClick Step ] [ text "Execute" ]
                ]
        )
