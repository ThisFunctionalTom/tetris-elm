module App exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String
import Dict
import Time exposing (Time)
import Tetromino exposing (..)
import Matrix exposing (..)
import Random.Pcg as Random
import Types exposing (..)
import Platform.Sub as Sub
import Keyboard exposing (KeyCode)


type alias State =
    { matrix : Matrix
    , falling : Maybe Falling
    }


init : ( State, Cmd Msg )
init =
    ( State Matrix.empty Nothing, Cmd.none )


type Msg
    = Tick Time
    | NewTetromino (Maybe Tetromino)
    | KeyDown KeyCode


nextFalling : Cmd Msg
nextFalling =
    Random.generate NewTetromino (Random.sample tetrominos)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Tick _ ->
            case state.falling of
                Nothing ->
                    ( state, nextFalling )

                Just falling ->
                    case moveDown state.matrix falling of
                        Nothing ->
                            ( { state | matrix = state.matrix |> addBlocks falling }, nextFalling )

                        Just next ->
                            ( { state | falling = Just next }, Cmd.none )

        NewTetromino maybeTetromino ->
            ( { state | falling = maybeTetromino |> Maybe.map (newFalling state.matrix) }, Cmd.none )

        KeyDown keyCode ->
            ( keyCode |> toKey |> Maybe.map (updateWithKey state) |> Maybe.withDefault state, Cmd.none )


updateWithKey : State -> Key -> State
updateWithKey state key =
    let
        transformation =
            case key of
                Left ->
                    moveLeft

                Right ->
                    moveRight

                Up ->
                    rotateRight

                Down ->
                    moveDown

                Space ->
                    softDrop
    in
        case state.falling |> Maybe.andThen (transformation state.matrix) of
            Just next ->
                { state | falling = Just next }

            Nothing ->
                state


softDrop : Matrix -> Falling -> Maybe Falling
softDrop matrix valid =
    case moveDown matrix valid of
        Just next ->
            softDrop matrix next

        Nothing ->
            Just valid


val : Int -> String
val value =
    toString value


vals : List Int -> String
vals values =
    values |> List.map toString |> String.join " "


playFieldBox : List Int
playFieldBox =
    [ 0, 0, 10, 22 ]


unitSize : Int
unitSize =
    30


view :
    State
    -> Html Msg
view state =
    let
        w =
            toString <| 10 * unitSize

        h =
            toString <| 22 * unitSize
    in
        svg
            [ width w
            , height h
            , viewBox <| vals playFieldBox
            ]
            [ rect [ x "0", y "0", width w, height h, color "black" ] []
            , viewBlocks state.matrix.blocks
            , state.falling |> Maybe.map viewFalling |> Maybe.withDefault (g [] [])
            ]


viewFalling : Falling -> Svg msg
viewFalling falling =
    Tetromino.blocks falling.offset falling.rotation falling.tetromino
        |> viewBlocks


viewBlocks : Blocks -> Svg msg
viewBlocks blocks =
    g []
        (blocks
            |> Dict.toList
            |> List.map viewBlock
        )


viewBlock : ( Offset, Color ) -> Svg msg
viewBlock ( ( row, col ), color ) =
    rect
        [ x <| toString col
        , y <| toString row
        , width "0.9"
        , height "0.9"
        , fill color
        ]
        []


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.batch
        [ Time.every (250 * Time.millisecond) (\t -> Tick t)
        , Keyboard.downs KeyDown
        ]
