module App exposing (..)

import Html exposing (Html, h1, div, text)
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


type alias Bag =
    { seed : Random.Seed
    , tetrominos : List Tetromino
    }


initialBag : Int -> Bag
initialBag seed =
    Bag (Random.initialSeed seed) tetrominos


spawn : Bag -> ( Tetromino, Bag )
spawn { seed, tetrominos } =
    case tetrominos of
        [] ->
            ( Tetromino.tetrominoT, Bag seed Tetromino.tetrominos )

        lastOne :: [] ->
            ( lastOne, Bag seed Tetromino.tetrominos )

        default :: rest ->
            let
                generator =
                    Random.sample tetrominos |> Random.map (Maybe.withDefault default)

                ( chosen, nextSeed ) =
                    Random.step generator seed
            in
                ( chosen, Bag nextSeed (List.filter ((/=) chosen) tetrominos) )


type alias State =
    { matrix : Matrix
    , falling : Falling
    , bag : Bag
    , score : Int
    }


init : Int -> ( State, Cmd Msg )
init seed =
    let
        ( tetromino, bag ) =
            spawn (initialBag seed)

        matrix =
            Matrix.empty

        falling =
            newFalling matrix tetromino
    in
        ( State matrix falling bag 0, Cmd.none )


type Msg
    = Tick Time
    | KeyDown KeyCode


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Tick _ ->
            case moveDown state.matrix state.falling of
                Nothing ->
                    let
                        ( count, matrix ) =
                            state.matrix |> addBlocks state.falling |> removeFullLines

                        ( tetromino, bag ) =
                            spawn state.bag
                    in
                        ( { state
                            | matrix = matrix
                            , score = state.score + count
                            , falling = newFalling state.matrix tetromino
                            , bag = bag
                          }
                        , Cmd.none
                        )

                Just next ->
                    ( { state | falling = next }, Cmd.none )

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
        case state.falling |> transformation state.matrix of
            Just next ->
                { state | falling = next }

            Nothing ->
                state


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
        div []
            [ h1 [] [ Html.text (toString state.score) ]
            , svg
                [ width w
                , height h
                , viewBox <| vals playFieldBox
                ]
                [ rect [ x "0", y "0", width w, height h, color "black" ] []
                , viewBlocks state.matrix.blocks
                , state.falling |> viewFalling
                ]
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
