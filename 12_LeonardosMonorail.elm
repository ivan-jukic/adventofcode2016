import Html exposing (..)
import Html.Events exposing ( onClick )

import String exposing ( toInt )
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

type Command
    = Cpy String String | Inc String | Dec String | Jnz String Int | Unknown


type alias Registry =
    { a: Int
    , b: Int
    , c: Int
    , d: Int
    }


initRegister: Registry
initRegister =
    --Registry 0 0 0 0 -- Part 1
    Registry 0 0 1 0 -- Part 2


type alias Model =
    { registry: Registry
    , cmds: List ( Int, String )
    , current: Int
    }


init: ( Model, Cmd Msg )
init =
    ( Model 
        initRegister
        ( input |> String.lines |> List.indexedMap (,) )
        0
    , Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- UPDATE

type Msg 
    = NoOp
    | NextCommand


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NextCommand ->
            let
                runCmd =
                    model.cmds |> List.filter ( \(i, cmd) -> i == model.current)

                ( ( newRegistry, cmdOffset ), action ) =
                    case runCmd of
                        ( i, cmd ) :: [] ->
                            ( process cmd model.registry
                            , Task.perform ( \_ -> NextCommand ) ( Task.succeed () )
                            --, Cmd.none
                            )
                        _ ->
                            let
                                d = Debug.log "registry" ( model.registry, model.current )
                            in
                                ( ( model.registry, 0 ), Cmd.none )
                
                newCurrent =
                    model.current + cmdOffset

                --d = Debug.log "registry" ( model.registry, model.current )
            in
                ( { model | registry = newRegistry, current = newCurrent }
                , action
                )


process: String -> Registry -> ( Registry, Int )
process cmdStr registry =
    let
        cmd =
            cmdStr |> parseCommand

        --d = Debug.log "cmd" cmd
    in
        case cmd of
            Inc reg ->
                ( registry |> incOrDec reg 1, 1 )
            Dec reg ->
                ( registry |> incOrDec reg -1, 1 )
            Cpy val reg ->
                ( registry |> cpy val reg, 1 )
            Jnz reg val ->
                ( registry, registry |> jnz reg val )
            _ ->
                ( registry, 1 )


incOrDec: String -> Int -> Registry -> Registry
incOrDec reg val registry =
    case reg of
        "a" -> { registry | a = registry.a + val }
        "b" -> { registry | b = registry.b + val }
        "c" -> { registry | c = registry.c + val }
        "d" -> { registry | d = registry.d + val }
        _ -> registry


cpy: String -> String -> Registry -> Registry
cpy val reg registry =
    let
        realVal =
            case val of
                "a" -> registry.a
                "b" -> registry.b
                "c" -> registry.c
                "d" -> registry.d
                _ -> val |> strToInt
    in
        case reg of
            "a" -> { registry | a = realVal }
            "b" -> { registry | b = realVal }
            "c" -> { registry | c = realVal }
            "d" -> { registry | d = realVal }
            _ -> registry


jnz: String -> Int -> Registry -> Int
jnz reg steps registry =
    let
        val =
            case reg of
                "a" -> registry.a
                "b" -> registry.b
                "c" -> registry.c
                "d" -> registry.d
                _ -> reg |> strToInt
    in
        case val /= 0 of
            True ->
                steps
            False ->
                1


parseCommand: String -> Command
parseCommand cmdStr =
    case ( String.words cmdStr ) of
        c :: valA :: valB :: [] ->
            case c of
                "cpy" ->
                    Cpy valA valB
                "jnz" ->
                    Jnz valA ( valB |> strToInt )
                _ ->
                    Unknown
        c :: reg :: [] ->
            case c of
                "inc" ->
                    Inc reg
                "dec" ->
                    Dec reg
                _ ->
                    Unknown
        _ ->
            Unknown


strToInt: String -> Int
strToInt str =
    str |> String.toInt |> Result.withDefault 0


-- VIEW

view: Model -> Html Msg
view model =
    div []
        [ div [] [ text "current registry state:" ]
        , ul []
            [ li [] [ text ( "a:" ++ (toString model.registry.a) )]
            , li [] [ text ( "b:" ++ (toString model.registry.b) )]
            , li [] [ text ( "c:" ++ (toString model.registry.c) )]
            , li [] [ text ( "d:" ++ (toString model.registry.d) )]
            ]
        , button [ onClick NextCommand ] [ text "Execute" ]
        ]


-- INPUT

input: String
input =
{-- }
    """cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a"""
--}

{--}
    """cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 16 c
cpy 17 d
inc a
dec d
jnz d -2
dec c
jnz c -5"""
--}