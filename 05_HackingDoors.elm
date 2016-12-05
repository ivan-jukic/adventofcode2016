import Html exposing (..)

import MD5


main: Program Never Model Msg
main = 
    Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL


type alias Model
    = Int


model : Model
model =
    0


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
        input =
            "ugkcyxxp"

        password = "d4cd2ee1"
            -- findPassword input 0 ""

        {--
        hash: ("ugkcyxxp702868","00000d82520c9edae5aff0c95131fa2f","d")
        hash: ("ugkcyxxp1776010","0000043e8f3f991deb83a48397ae1e7e","4")
        hash: ("ugkcyxxp8421983","00000c6ef1f544e7423e5234fc295e2c","c")
        hash: ("ugkcyxxp8744114","00000db7c2dd1100436ce9ee71df4a6f","d")
        hash: ("ugkcyxxp8845282","000002c7d4257f42319af0bf3d255eac","2")
        hash: ("ugkcyxxp9268910","00000e94d4c249f954b637a1dc5eaa67","e")
        hash: ("ugkcyxxp9973527","00000e1355a341447c1b13967dacdfeb","e")
        hash: ("ugkcyxxp10253166","0000012e0248931afa8e7a3810442e98","1")
        --}

        password2 = "f2c730e5"
        {--
            findPassword2 input 0 []
                |> List.sortWith
                    ( \(i1, c1) (i2, c2)-> 
                        case compare i1 i2 of
                            LT -> LT
                            EQ -> EQ
                            GT -> GT
                    )
                |> List.map ( \( i, c ) -> c )
                |> String.join ""
        --}

        {--
        hash2: ("ugkcyxxp1776010","0000043e8f3f991deb83a48397ae1e7e",(4,"3"))
        hash2: ("ugkcyxxp8845282","000002c7d4257f42319af0bf3d255eac",(2,"c"))
        hash2: ("ugkcyxxp10253166","0000012e0248931afa8e7a3810442e98",(1,"2"))
        hash2: ("ugkcyxxp13176820","0000050aea0f692b0409740c1b7c021b",(5,"0"))
        hash2: ("ugkcyxxp13604912","00000751932293e0a91438646f8f6117",(7,"5"))
        hash2: ("ugkcyxxp14375655","000000f59a581a0ab3896663bf5bee0a",(0,"f"))
        hash2: ("ugkcyxxp14578671","000006e2abaa3d7bff2e8e6a35e078a6",(6,"e"))
        hash2: ("ugkcyxxp25176241","0000037455348b6f39fa24219c96c6f4",(3,"7"))
        --}
    in
        div []
            [ div [] [ text ( "My password is: " ++ password ) ] 
            , div [] [ text ( "Second password is: " ++ password2 ) ]
            ]


findPassword: String -> Int -> String -> String
findPassword input count current =
    if String.length current == 8 || count > 100000000 then
        current
    else
        let
            hash =
                MD5.hex ( input ++ ( toString count ) )

            fiveFirst =
                String.left 5 hash

            newCharInPassword =
                if "00000" == fiveFirst then
                    hash |> String.dropLeft 5 |> String.left 1
                else
                    ""
        in
            findPassword input ( count + 1 ) ( current ++ newCharInPassword )


findPassword2: String -> Int -> List ( Int, String ) -> List ( Int, String )
findPassword2 input count current =
    if List.length current == 8 || count > 200000000 then
        current
    else
        let
            newInput =
                input ++ ( toString count )

            hash =
                MD5.hex newInput

            fiveFirst =
                String.left 5 hash

            newCurrent =
                if "00000" == fiveFirst then
                    let
                        idx =
                            ( hash |> String.dropLeft 5 |> String.left 1 )
                                |> String.toInt
                                |> Result.withDefault -1

                        containsThisChar =
                            List.any ( \( i, c ) -> i == idx ) current

                        newCodePart =
                            if idx > -1 && idx < 8 && not containsThisChar then
                                let
                                    char =
                                        hash |> String.dropLeft 6 |> String.left 1

                                    d = Debug.log "hash2" ( newInput, hash, (idx, char) )
                                in
                                    [ ( idx, char ) ]
                            else
                                []

                    in
                        List.append current newCodePart
                else
                    current

            t =
                if count % 1000000 == 0 then
                    Debug.log "checkpoint" count
                else
                    0
        in
            findPassword2 input ( count + 1 ) newCurrent