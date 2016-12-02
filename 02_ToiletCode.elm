import Html exposing (..)


main: Program Never Model Msg
main = 
    Html.beginnerProgram { model = model, view = view, update = update }


-- TYPES

type alias Moves =
    { u: ( Int, Int )
    , d: ( Int, Int )
    , r: ( Int, Int )
    , l: ( Int, Int )
    }


type alias Model =
    { i: Int
    , j: Int
    }


moves: Moves
moves =
    { u = ( -1, 0 )
    , d = ( 1, 0 )
    , r = ( 0, 1 )
    , l = ( 0, -1 )
    }


model : Model
model =
    { i = 1
    , j = 1
    }


-- UPDATE

type Msg = NoOp

update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


-- VIEW
-- http://adventofcode.com/2016/day/2

view : Model -> Html Msg
view model =
    let
        codeLines =
            puzzleInput |> String.split "\n"

        endModels =
            codeLines |> List.foldl ( \input models -> getModelForLine input models ( Model 1 1 ) False ) []
        
        altModels =
            codeLines |> List.foldl ( \input models -> getModelForLine input models ( Model 2 0 ) True ) [] 

        code =
            endModels |>
                List.foldl ( \pos prev ->
                    List.append prev [ toString (pos.i * 3 + pos.j + 1) ]
                ) []

        code2 =
            altModels |>
                List.foldl ( \pos prev ->
                    let
                        num =
                            pos.i * 5 + pos.j + 1

                        key =
                            if 17 == num then "A"
                            else if 18 == num then "B"
                            else if 19 == num then "C"
                            else if 23 == num then "D"
                            else if pos.i == 0 then
                                toString ( num - 2 )
                            else if pos.i == 1 then
                                toString ( num - 5 )
                            else if pos.i == 2 then
                                toString ( num - 6 )
                            else
                                toString num
                    in
                        List.append prev [ key ]
                ) []
    in
        div []
            [ div [] [ text ( "Toilet code: " ++ ( String.join "" code ) ) ]
            , div [] [ text ( "Real toilet code: " ++ ( String.join "" code2 ) ) ]
            ]


getModelForLine: String -> List Model -> Model -> Bool -> List Model
getModelForLine input models initialModel isRestricted =
    let
        startModel =
            models |> List.reverse |> List.head |> Maybe.withDefault initialModel

        moves =
            input |> String.split ""

        actionFn =
            if isRestricted then restrictedMoveOnce else moveOnce

        endModel =
            moves |> List.foldl actionFn startModel
    in
        List.append models [ endModel ]


moveOnce: String -> Model -> Model
moveOnce move pos =
    let
        ( di, dj ) =
            getMoveDeltas move

        ( i, j ) =
            ( pos.i + di, pos.j + dj )
    in
        { pos
        | i = if i > -1 && i < 3 then i else pos.i
        , j = if j > -1 && j < 3 then j else pos.j
        }


restrictedMoveOnce: String -> Model -> Model
restrictedMoveOnce move pos =
    let
        ( di, dj ) =
            getMoveDeltas move

        ( i, j ) =
            ( pos.i + di, pos.j + dj )

        restricted =
            [ ( 0, 0 ), ( 0, 4 ), ( 4, 0 ), ( 4, 4 ) ]
                |> List.foldl
                    ( \ ( x, y ) prev ->
                        let
                            ( x1, y1 ) =
                                ( if x + 1 < 5 then x + 1 else x - 1 , y )

                            ( x2, y2 ) =
                                ( x , if y + 1 < 5 then y + 1 else y - 1 )
                        in
                            List.append prev [ ( x, y ), ( x1, y1 ), ( x2, y2 ) ]
                    ) []
    in
        { pos
        | i = if i > -1 && i < 5 && not ( List.member ( i, j ) restricted ) then i else pos.i
        , j = if j > -1 && j < 5 && not ( List.member ( i, j ) restricted ) then j else pos.j
        }


getMoveDeltas: String -> ( Int, Int )
getMoveDeltas m =
    if m == "U" then moves.u
    else if m == "D" then moves.d
    else if m == "L" then moves.l
    else if m == "R" then moves.r
    else ( 0, 0 )


-- INPUT


puzzleInput: String
puzzleInput =
{--
    """ULL
RRDDD
LURDL
UUUUD"""
--}
{--}
    """RDLULDLDDRLLLRLRULDRLDDRRRRURLRLDLULDLDLDRULDDLLDRDRUDLLDDRDULLLULLDULRRLDURULDRUULLLUUDURURRDDLDLDRRDDLRURLLDRRRDULDRULURURURURLLRRLUDULDRULLDURRRLLDURDRRUUURDRLLDRURULRUDULRRRRRDLRLLDRRRDLDUUDDDUDLDRUURRLLUDUDDRRLRRDRUUDUUULDUUDLRDLDLLDLLLLRRURDLDUURRLLDLDLLRLLRULDDRLDLUDLDDLRDRRDLULRLLLRUDDURLDLLULRDUUDRRLDUDUDLUURDURRDDLLDRRRLUDULDULDDLLULDDDRRLLDURURURUUURRURRUUDUUURULDLRULRURDLDRDDULDDULLURDDUDDRDRRULRUURRDDRLLUURDRDDRUDLUUDURRRLLRR
RDRRLURDDDDLDUDLDRURRLDLLLDDLURLLRULLULUUURLDURURULDLURRLRULDDUULULLLRLLRDRRUUDLUUDDUDDDRDURLUDDRULRULDDDLULRDDURRUURLRRLRULLURRDURRRURLDULULURULRRLRLUURRRUDDLURRDDUUDRDLLDRLRURUDLDLLLLDLRURDLLRDDUDDLDLDRRDLRDRDLRRRRUDUUDDRDLULUDLUURLDUDRRRRRLUUUDRRDLULLRRLRLDDDLLDLLRDDUUUUDDULUDDDUULDDUUDURRDLURLLRUUUUDUDRLDDDURDRLDRLRDRULRRDDDRDRRRLRDULUUULDLDDDUURRURLDLDLLDLUDDLDLRUDRLRLDURUDDURLDRDDLLDDLDRURRULLURULUUUUDLRLUUUDLDRUDURLRULLRLLUUULURLLLDULLUDLLRULRRLURRRRLRDRRLLULLLDURDLLDLUDLDUDURLURDLUURRRLRLLDRLDLDRLRUUUDRLRUDUUUR
LLLLULRDUUDUUDRDUUURDLLRRLUDDDRLDUUDDURLDUDULDRRRDDLLLRDDUDDLLLRRLURDULRUUDDRRDLRLRUUULDDULDUUUDDLLDDDDDURLDRLDDDDRRDURRDRRRUUDUUDRLRRRUURUDURLRLDURDDDUDDUDDDUUDRUDULDDRDLULRURDUUDLRRDDRRDLRDLRDLULRLLRLRLDLRULDDDDRLDUURLUUDLLRRLLLUUULURUUDULRRRULURUURLDLLRURUUDUDLLUDLDRLLRRUUDDRLUDUDRDDRRDDDURDRUDLLDLUUDRURDLLULLLLUDLRRRUULLRRDDUDDDUDDRDRRULURRUUDLUDLDRLLLLDLUULLULLDDUDLULRDRLDRDLUDUDRRRRLRDLLLDURLULUDDRURRDRUDLLDRURRUUDDDRDUUULDURRULDLLDLDLRDUDURRRRDLDRRLUDURLUDRRLUDDLLDUULLDURRLRDRLURURLUUURRLUDRRLLULUULUDRUDRDLUL
LRUULRRUDUDDLRRDURRUURDURURLULRDUUDUDLDRRULURUDURURDRLDDLRUURLLRDLURRULRRRUDULRRULDLUULDULLULLDUDLLUUULDLRDRRLUURURLLUUUDDLLURDUDURULRDLDUULDDRULLUUUURDDRUURDDDRUUUDRUULDLLULDLURLRRLRULRLDLDURLRLDLRRRUURLUUDULLLRRURRRLRULLRLUUDULDULRDDRDRRURDDRRLULRDURDDDDDLLRRDLLUUURUULUDLLDDULDUDUUDDRURDDURDDRLURUDRDRRULLLURLUULRLUDUDDUUULDRRRRDLRLDLLDRRDUDUUURLRURDDDRURRUDRUURUUDLRDDDLUDLRUURULRRLDDULRULDRLRLLDRLURRUUDRRRLRDDRLDDLLURLLUDL
ULURLRDLRUDLLDUDDRUUULULUDDDDDRRDRULUDRRUDLRRRLUDLRUULRDDRRLRUDLUDULRULLUURLLRLLLLDRDUURDUUULLRULUUUDRDRDRUULURDULDLRRULUURURDULULDRRURDLRUDLULULULUDLLUURULDLLLRDUDDRRLULUDDRLLLRURDDLDLRLLLRDLDRRUUULRLRDDDDRUDRUULDDRRULLDRRLDDRRUDRLLDUDRRUDDRDLRUDDRDDDRLLRDUULRDRLDUDRLDDLLDDDUUDDRULLDLLDRDRRUDDUUURLLUURDLULUDRUUUDURURLRRDULLDRDDRLRDULRDRURRUDLDDRRRLUDRLRRRRLLDDLLRLDUDUDDRRRUULDRURDLLDLUULDLDLDUUDDULUDUDRRDRLDRDURDUULDURDRRDRRLLRLDLU"""
--}