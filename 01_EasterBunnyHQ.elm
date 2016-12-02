import Html exposing (..)

import Maybe exposing (withDefault)

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

        d = Debug.log "moves" moves

        endModel =
            moves |> newPos model
    in
        div []
            [ text ( "model one " ++ ( toString endModel ) )
            ]
    
    
newPos: Model -> List Movement -> Model
newPos model movements =
    List.foldl reduce model movements
  
  
reduce: Movement -> Model -> Model
reduce move model =
    let
        newO =
            case move.dir of
                Right ->
                    rem ( model.o + 1 ) 4
                Left ->
                    if (model.o - 1) < 0 then 0 else model.o - 1

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

                --d = Debug.log "parts" (dir, amount)
            in
                { dir = if "R" == dir then Right else Left
                , amount = amount
                }
        )


{--
d1 = 5
        d2 = 2
        d3 = 12

        modelOne =
            [ Movement Right 2, Movement Left 3 ]
                |> newPos model

        modelTwo = 
            [ Movement Right 2, Movement Right 2, Movement Right 2 ]
                |> newPos model

        modelThree =
            [ Movement Right 5, Movement Left 5, Movement Right 5 , Movement Right 3 ]
                |> newPos model

        ( minX, maxX ) =
            ( List.maximum [ modelOne.x - d1, modelTwo.x - d2, modelThree.x - d3 ] |> withDefault -1
            , List.minimum [ modelOne.x + d1, modelTwo.x + d2, modelThree.x + d3 ] |> withDefault 1
            )

        ( minY, maxY ) =
            ( List.maximum [ modelOne.y - d1, modelTwo.y - d2, modelThree.y - d3 ] |> withDefault -1
            , List.minimum [ modelOne.y + d1, modelTwo.y + d2, modelThree.y + d3 ] |> withDefault 1
            )

        hasIntersect =
            minX < maxX && minY < maxY

        minDistance =
            if hasIntersect then
                List.minimum [ abs minX, abs minY, abs maxX, abs maxY ]
            else
                Nothing
    in
        div []
            [ div [] [ text ( "Position one: " ++ (toString modelOne ) ) ]
            , div [] [ text ( "Position two: " ++ (toString modelTwo ) ) ]
            , div [] [ text ( "Position three: " ++ (toString modelThree ) ) ]
            , ( if not hasIntersect then
                    div [] [ text "unable to calculate" ]
                else
                    div[]
                        [ div [] [ text ( "Min (X, Y) intersect: (" ++ (toString minX) ++ ", " ++ (toString minY) ++ ")" ) ]
                        , div [] [ text ( "Max (X, Y) intersect: (" ++ (toString maxX) ++ ", " ++ (toString maxY) ++ ")" ) ]
                        , div [] [ text ( "Minimum distance to HQ is: " ++ (toString minDistance) ) ]
                        ]
            )
            ]
--}