import Html exposing (..)


main: Program Never Model Msg
main = 
    Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type Move =
    Left | Right

type alias Movement =
    { dir: Move
    , amount: Int
    }

type alias Orientation =
    { north: Int
    , east: Int
    , south: Int
    , west: Int
    }

orientation: Orientation
orientation =
    { north = 0
    , east = 1
    , south = 2
    , west = 3
    }

type alias Model = 
    { x: Int
    , y: Int
    , o: Int
    }

model : Model
model =
    { x = 0
    , y = 0
    , o = 0
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
        moves =
            requiredMoves

        endModel =
            moves |> newPos model

        ( twiceX, twiceY ) =
            moves |> findFirstLocationVisitedTwice model

        d = Debug.log "locs" ( endModel, (twiceX, twiceY) )
    in
        div []
            [ div [] [ text ( "Minimum distance to Bunny HQ is: " ++ ( toString ( abs endModel.x + abs endModel.y ) ) ) ]
            , div [] [ text ( "First location visited twice distance: " ++ ( toString ( abs twiceX + abs twiceY ) ) ) ]
            ]
    
    
newPos: Model -> List Movement -> Model
newPos model movements =
    List.foldl reduce model movements
  

findFirstLocationVisitedTwice: Model -> List Movement -> ( Int, Int )
findFirstLocationVisitedTwice model movements =
    let
        findAllPoints =
            ( \cmd points ->
                let
                    lastPoint =
                        points |> List.reverse |> List.head |> Maybe.withDefault (Model 0 0 0)

                    newPoints =
                        List.range 1 cmd.amount
                            |> List.map
                                ( \amount ->
                                    reduce { dir = cmd.dir, amount = amount } lastPoint
                                )
                in
                    List.append points newPoints
            )

        allLocations =
            List.foldl findAllPoints [ model ] movements

        points =
            List.map ( \i -> ( i.x, i.y ) ) allLocations

        repeatedLocations =
            List.indexedMap (,) points
                |> List.filter 
                    ( \( idx, ( x, y ) ) ->
                        let
                            otherItems =
                                List.drop ( idx + 1 ) points
                        in
                            List.member ( x, y ) otherItems
                    )
            
        ( idx, firstPoint ) =
            repeatedLocations |> List.head |> Maybe.withDefault ( 0, ( 0, 0 ) )
    in
        firstPoint


reduce: Movement -> Model -> Model
reduce move model =
    let
        newO =
            case move.dir of
                Right ->
                    rem ( model.o + 1 ) 4
                Left ->
                    if (model.o - 1) < 0 then 3 else model.o - 1

        getMultiplier =
            (\c1 c2 ->
                if c1 then 1 else if c2 then -1 else 0
            )

        ( multiX, multiY ) =
            ( getMultiplier (newO == orientation.east) (newO == orientation.west)
            , getMultiplier (newO == orientation.north) (newO == orientation.south)
            )

        ( newX, newY ) = 
            ( model.x + ( multiX * move.amount )
            , model.y + ( multiY * move.amount )
            )
    in
        { model | x = newX, y = newY, o = newO }


requiredMoves: List Movement
requiredMoves =
    [ "R4", "R1", "L2", "R1", "L1", "L1", "R1", "L5", "R1", "R5", "L2", "R3", "L3", "L4", "R4", "R4", "R3", "L5", "L1", "R5", "R3", "L4", "R1", "R5", "L1", "R3", "L2"
    , "R3", "R1", "L4", "L1", "R1", "L1", "L5", "R1", "L2", "R2", "L3", "L5", "R1", "R5", "L1", "R188", "L3", "R2", "R52", "R5", "L3", "R79", "L1", "R5", "R186", "R2"
    , "R1", "L3", "L5", "L2", "R2", "R4", "R5", "R5", "L5", "L4", "R5", "R3", "L4", "R4", "L4", "L4", "R5", "L4", "L3", "L1", "L4", "R1", "R2", "L5", "R3", "L4", "R3"
    , "L3", "L5", "R1", "R1", "L3", "R2", "R1", "R2", "R2", "L4", "R5", "R1", "R3", "R2", "L2", "L2", "L1", "R2", "L1", "L3", "R5", "R1", "R4", "R5", "R2", "R2", "R4"
    , "R4", "R1", "L3", "R4", "L2", "R2", "R1", "R3", "L5", "R5", "R2", "R5", "L1", "R2", "R4", "L1", "R5", "L3", "L3", "R1", "L4", "R2", "L2", "R1", "L1", "R4", "R3"
    , "L2", "L3", "R3", "L2", "R1", "L4", "R5", "L1", "R5", "L2", "L1", "L5", "L2", "L5", "L2", "L4", "L2", "R3"
    ] |>
    List.map
        (\m ->
            let
                dir =
                    String.left 1 m

                amount =
                    m |> String.dropLeft 1 |> String.toInt |> Result.withDefault 0
            in
                { dir = if "R" == dir then Right else Left
                , amount = amount
                }
        )
