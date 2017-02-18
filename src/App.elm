module App exposing (..)

import Html exposing (Html, h1, h2, div, text)
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


initBag : Random.Seed -> Bag
initBag seed =
    Bag seed tetrominos


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


type alias GameOnState =
    { matrix : Matrix
    , falling : Falling
    , bag : Bag
    , score : Int
    }


type alias GameOverState =
    { score : Maybe Int
    , seed : Random.Seed
    }


type State
    = GameOn GameOnState
    | GameOver GameOverState


init : Int -> ( State, Cmd Msg )
init intSeed =
    ( GameOver <| GameOverState Nothing (Random.initialSeed intSeed), Cmd.none )


startNew : Random.Seed -> ( State, Cmd Msg )
startNew seed =
    let
        ( tetromino, bag ) =
            spawn (initBag seed)

        matrix =
            Matrix.empty

        falling =
            newFalling matrix tetromino
    in
        ( GameOn <| GameOnState matrix falling bag 0, Cmd.none )


type Msg
    = Tick Time
    | KeyDown KeyCode


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case state of
        GameOn gos ->
            updateGameOn msg gos

        GameOver gos ->
            updateGameOver msg gos


updateGameOver : Msg -> GameOverState -> ( State, Cmd Msg )
updateGameOver msg state =
    case msg of
        KeyDown keyCode ->
            case toKey keyCode of
                Just F2 ->
                    startNew state.seed

                _ ->
                    ( GameOver state, Cmd.none )

        _ ->
            ( GameOver state, Cmd.none )


updateGameOn : Msg -> GameOnState -> ( State, Cmd Msg )
updateGameOn msg state =
    case msg of
        Tick _ ->
            case moveDown state.matrix state.falling of
                Nothing ->
                    let
                        ( count, matrix ) =
                            state.matrix |> addBlocks state.falling |> removeFullLines

                        ( tetromino, bag ) =
                            spawn state.bag

                        falling =
                            newFalling state.matrix tetromino
                    in
                        if isValid state.matrix falling then
                            ( GameOn
                                { state
                                    | matrix = matrix
                                    , score = state.score + count
                                    , falling = falling
                                    , bag = bag
                                }
                            , Cmd.none
                            )
                        else
                            ( GameOver <| GameOverState (Just state.score) state.bag.seed, Cmd.none )

                Just next ->
                    ( GameOn { state | falling = next }, Cmd.none )

        KeyDown keyCode ->
            toKey keyCode
                |> Maybe.map (updateWithKey state)
                |> Maybe.withDefault ( GameOn state, Cmd.none )


updateWithKey : GameOnState -> Key -> ( State, Cmd Msg )
updateWithKey state key =
    let
        transform f =
            case state.falling |> f state.matrix of
                Just next ->
                    ( GameOn { state | falling = next }, Cmd.none )

                Nothing ->
                    ( GameOn state, Cmd.none )
    in
        case key of
            F2 ->
                startNew state.bag.seed

            Left ->
                transform moveLeft

            Right ->
                transform moveRight

            Up ->
                transform rotateRight

            Down ->
                transform moveDown

            Space ->
                transform softDrop


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


view : State -> Html Msg
view state =
    case state of
        GameOver gos ->
            viewGameOver gos

        GameOn gos ->
            viewGameOn gos


viewGameOver : GameOverState -> Html Msg
viewGameOver state =
    div
        []
        [ h1 [] [ Html.text <| (state.score |> Maybe.map (toString >> (++) "Game over ") |> Maybe.withDefault "") ]
        , h2 [] [ Html.text "press F2 to start" ]
        ]


viewGameOn : GameOnState -> Html Msg
viewGameOn state =
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
    case state of
        GameOn _ ->
            Sub.batch
                [ Time.every (250 * Time.millisecond) (\t -> Tick t)
                , Keyboard.downs KeyDown
                ]

        GameOver _ ->
            Keyboard.downs KeyDown
