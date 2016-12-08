import Html exposing (..)
import Html.Attributes exposing (..)

import Regex


main: Program Never Model Msg
main = 
    Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type Command
    = Rect ( Int, Int )
    | RotCol ( Int, Int )
    | RotRow ( Int, Int )
    | CmdNoOp


type alias Model =
    { x: Int
    , y: Int
    }


model : Model
model =
    { x = 50
    , y = 6
    }


-- UPDATE

type Msg = NoOp

update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


-- VIEW

view : Model -> Html Msg
view model =
    let
        display =
            List.repeat ( model.x * model.y ) 0 |> List.indexedMap (,)

        finalDisplay =
            mainInput
                |> String.lines
                |> List.map parseCommand
                |> List.foldl ( \cmd dsp ->
                    case cmd of
                        Rect ( x, y ) ->
                            fillRect dsp x y
                        RotCol ( x, d ) ->
                            rotCol dsp x d
                        RotRow ( y, d ) ->
                            rotRow dsp y d
                        _ ->
                            dsp
                ) display

        totalLit =
            finalDisplay
                |> List.map ( \(i, v) -> v )
                |> List.sum

        pixels =
            List.range 0 ( model.y - 1 )
                |> List.foldl
                    ( \row current ->
                        current ++ [ ( finalDisplay |> List.drop ( row * model.x ) |> List.take model.x ) ]
                    ) []

        blockStyle =
            ( \isOn ->
                [ ( "display", "block")
                , ( "float", "left" )
                , ( "width", "20px" )
                , ( "height", "20px" )
                , ( "background-color", ( if isOn then "#5bb8ff" else "#DDD" ) )
                , ( "border-bottom", "1px solid #FFF" )
                , ( "border-right", "1px solid #FFF" )
                ]
            )
    in
        div []
            [ div [] [ text ( "Grid, number of lit pixels: " ++ ( toString totalLit ) ) ]
            , div []
                ( pixels
                    |> List.map ( \row ->
                        div [ style [ ( "overflow", "hidden" ) ] ]
                            ( row 
                                |> List.map ( \(i, v) ->
                                    span [ style ( blockStyle ( v == 1 ) ) ]
                                        [ ]
                                )
                            )
                    )
                )
            ]


fillRect: List (Int, Int ) -> Int -> Int -> List (Int, Int)
fillRect display x y =
    let
        indices = List.range 0 ( y - 1 )
            |> List.foldl ( \row idx ->
                List.append idx
                    ( List.range ( row * model.x ) ( row * model.x + x - 1 ) ) ) []
    in
        display |> List.map
            ( \( idx, v ) ->
                if ( List.member idx indices ) then ( idx, 1 ) else ( idx, v ) )


rotCol: List (Int, Int) -> Int -> Int -> List (Int, Int)
rotCol display x d =
    List.range 0 ( model.y - 1 )
        |> List.foldl ( \row idx -> List.append idx [ row * model.x + x ]) []
        |> rotate display (model.y - ( d % model.y ))


rotRow: List (Int, Int) -> Int -> Int -> List (Int, Int)
rotRow display y d =
    List.range ( y * model.x ) ( ( y + 1 ) * model.x - 1 )
        |> rotate display (model.x - ( d % model.x ))


rotate: List (Int, Int) -> Int -> List Int -> List (Int, Int)
rotate display firstHalfLength indices =
    let
        ( idx, vals ) =
            display
                |> List.filter ( \(i, v) -> List.member i indices )
                |> List.unzip

        rotatedVals =
            List.append
                ( vals |> List.drop firstHalfLength )
                ( vals |> List.take firstHalfLength )

        newIdxVals =
            rotatedVals |> List.map2 ( \i v -> ( i, v ) ) idx
    in
        display |>
            List.map ( \(i, v) ->
                ( i, newIdxVals |> List.foldl ( \(ni, nv) c -> if ni == i then nv else c ) v )
            )


parseCommand: String -> Command
parseCommand cmdString =
    let
        rgx =
            [ "(rect)\\s+(\\d+)x(\\d+)"
            , "(rotate)\\s+((row)\\s+y=|(column)\\s+x=){1}(\\d+)\\s+by\\s+(\\d+)"
            ]

        matches = rgx
            |> List.concatMap ( \r -> Regex.find Regex.All ( Regex.regex r ) cmdString )
            |> List.foldl ( \m c -> m.submatches ) []
            |> List.filter ( \v -> v /= Nothing )
            |> List.map ( \v -> v |> Maybe.withDefault "" )
    in
        case ( List.head matches ) of
            Just c ->
                if c == "rect" then
                    Rect ( getXY ( List.drop 1 matches ) )
                else if c == "rotate" then
                    parseRot ( List.drop 2 matches )
                else
                    CmdNoOp
            Nothing ->
                CmdNoOp


parseRot: List String -> Command
parseRot args =
    case ( args |> List.head ) of
        Just t ->
            if t == "column" then
                RotCol ( getXY ( List.drop 1 args ) )
            else if t == "row" then
                RotRow ( getXY ( List.drop 1 args ) )
            else
                CmdNoOp
        Nothing ->
            CmdNoOp


getXY: List String -> ( Int, Int )
getXY args =
    let
        xy =
            args |> List.map ( \v -> v |> String.toInt |> Result.withDefault 0 )
    in
        ( xy |> List.head |> Maybe.withDefault 0
        , xy |> List.reverse |> List.head |> Maybe.withDefault 0
        )


-- INPUT

mainInput: String
mainInput =
{-- }
    """rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1"""
--}
{--}
    """rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 6
rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 5
rect 2x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 4
rect 1x1
rotate row y=0 by 3
rect 2x1
rotate row y=0 by 7
rect 3x1
rotate row y=0 by 3
rect 1x1
rotate row y=0 by 3
rect 1x2
rotate row y=1 by 13
rotate column x=0 by 1
rect 2x1
rotate row y=0 by 5
rotate column x=0 by 1
rect 3x1
rotate row y=0 by 18
rotate column x=13 by 1
rotate column x=7 by 2
rotate column x=2 by 3
rotate column x=0 by 1
rect 17x1
rotate row y=3 by 13
rotate row y=1 by 37
rotate row y=0 by 11
rotate column x=7 by 1
rotate column x=6 by 1
rotate column x=4 by 1
rotate column x=0 by 1
rect 10x1
rotate row y=2 by 37
rotate column x=19 by 2
rotate column x=9 by 2
rotate row y=3 by 5
rotate row y=2 by 1
rotate row y=1 by 4
rotate row y=0 by 4
rect 1x4
rotate column x=25 by 3
rotate row y=3 by 5
rotate row y=2 by 2
rotate row y=1 by 1
rotate row y=0 by 1
rect 1x5
rotate row y=2 by 10
rotate column x=39 by 1
rotate column x=35 by 1
rotate column x=29 by 1
rotate column x=19 by 1
rotate column x=7 by 2
rotate row y=4 by 22
rotate row y=3 by 5
rotate row y=1 by 21
rotate row y=0 by 10
rotate column x=2 by 2
rotate column x=0 by 2
rect 4x2
rotate column x=46 by 2
rotate column x=44 by 2
rotate column x=42 by 1
rotate column x=41 by 1
rotate column x=40 by 2
rotate column x=38 by 2
rotate column x=37 by 3
rotate column x=35 by 1
rotate column x=33 by 2
rotate column x=32 by 1
rotate column x=31 by 2
rotate column x=30 by 1
rotate column x=28 by 1
rotate column x=27 by 3
rotate column x=26 by 1
rotate column x=23 by 2
rotate column x=22 by 1
rotate column x=21 by 1
rotate column x=20 by 1
rotate column x=19 by 1
rotate column x=18 by 2
rotate column x=16 by 2
rotate column x=15 by 1
rotate column x=13 by 1
rotate column x=12 by 1
rotate column x=11 by 1
rotate column x=10 by 1
rotate column x=7 by 1
rotate column x=6 by 1
rotate column x=5 by 1
rotate column x=3 by 2
rotate column x=2 by 1
rotate column x=1 by 1
rotate column x=0 by 1
rect 49x1
rotate row y=2 by 34
rotate column x=44 by 1
rotate column x=40 by 2
rotate column x=39 by 1
rotate column x=35 by 4
rotate column x=34 by 1
rotate column x=30 by 4
rotate column x=29 by 1
rotate column x=24 by 1
rotate column x=15 by 4
rotate column x=14 by 1
rotate column x=13 by 3
rotate column x=10 by 4
rotate column x=9 by 1
rotate column x=5 by 4
rotate column x=4 by 3
rotate row y=5 by 20
rotate row y=4 by 20
rotate row y=3 by 48
rotate row y=2 by 20
rotate row y=1 by 41
rotate column x=47 by 5
rotate column x=46 by 5
rotate column x=45 by 4
rotate column x=43 by 5
rotate column x=41 by 5
rotate column x=33 by 1
rotate column x=32 by 3
rotate column x=23 by 5
rotate column x=22 by 1
rotate column x=21 by 2
rotate column x=18 by 2
rotate column x=17 by 3
rotate column x=16 by 2
rotate column x=13 by 5
rotate column x=12 by 5
rotate column x=11 by 5
rotate column x=3 by 5
rotate column x=2 by 5
rotate column x=1 by 5"""
--}
