module App exposing (..)

import Html exposing (Html, h1, h2, div, text)
import Time exposing (Time)
import Matrix exposing (..)
import Random.Pcg as Random
import Types exposing (..)
import Platform.Sub as Sub
import Keyboard exposing (KeyCode)
import TetrominoBag exposing (..)


type alias GameOnState =
    { matrix : Matrix
    , falling : Falling
    , bag : Bag
    , score : Int
    }


type alias GameOverState =
    { score : Int
    , matrix : Matrix
    , seed : Random.Seed
    }


type State
    = HomeScreen Random.Seed
    | GameOn GameOnState
    | GameOver GameOverState


init : Int -> ( State, Cmd Msg )
init intSeed =
    ( HomeScreen (Random.initialSeed intSeed), Cmd.none )


startNew : Random.Seed -> State
startNew seed =
    let
        ( tetromino, bag ) =
            spawn (initBag seed)

        matrix =
            Matrix.empty

        falling =
            newFalling matrix tetromino
    in
        GameOn <| GameOnState matrix falling bag 0


type Msg
    = Tick Time
    | KeyDown KeyCode


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    ( simpleUpdate msg state, Cmd.none )


simpleUpdate : Msg -> State -> State
simpleUpdate msg state =
    case state of
        HomeScreen seed ->
            case msg of
                KeyDown keyCode ->
                    case toKey keyCode of
                        Just F2 ->
                            startNew seed

                        _ ->
                            HomeScreen seed

                _ ->
                    HomeScreen seed

        GameOn gos ->
            updateGameOn msg gos

        GameOver gos ->
            updateGameOver msg gos


updateGameOver : Msg -> GameOverState -> State
updateGameOver msg state =
    case msg of
        KeyDown keyCode ->
            case toKey keyCode of
                Just F2 ->
                    startNew state.seed

                _ ->
                    GameOver state

        _ ->
            GameOver state


updateGameOn : Msg -> GameOnState -> State
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
                            GameOn
                                { state
                                    | matrix = matrix
                                    , score = state.score + count
                                    , falling = falling
                                    , bag = bag
                                }
                        else
                            GameOver <| GameOverState state.score state.matrix state.bag.seed

                Just next ->
                    GameOn { state | falling = next }

        KeyDown keyCode ->
            toKey keyCode
                |> Maybe.map (updateWithKey state)
                |> Maybe.withDefault (GameOn state)


updateWithKey : GameOnState -> Key -> State
updateWithKey state key =
    let
        transform f =
            case state.falling |> f state.matrix of
                Just next ->
                    GameOn { state | falling = next }

                Nothing ->
                    GameOn state
    in
        case key of
            F2 ->
                startNew state.bag.seed

            Left ->
                transform moveLeft

            Right ->
                transform moveRight

            Up ->
                transform rotateLeft

            Down ->
                transform moveDown

            Space ->
                transform softDrop


val : Int -> String
val value =
    toString value


cellSize : Int
cellSize =
    30


view : State -> Html Msg
view state =
    case state of
        HomeScreen _ ->
            viewHomeScreen

        GameOver gos ->
            viewGameOver gos

        GameOn gos ->
            viewGameOn gos


viewHomeScreen : Html Msg
viewHomeScreen =
    div []
        [ h1 [] [ Html.text "Press F2 to start." ]
        , viewSample cellSize
        ]


viewGameOver : GameOverState -> Html Msg
viewGameOver state =
    div
        []
        [ h1 [] [ Html.text <| "Game over " ++ (toString state.score) ]
        , h2 [] [ Html.text "press F2 to start" ]
        , viewMatrix cellSize state.matrix []
        ]


viewGameOn : GameOnState -> Html Msg
viewGameOn state =
    div []
        [ h1 []
            [ Html.text (toString state.score) ]
        , viewMatrix cellSize state.matrix [ state.falling ]
        ]


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

        HomeScreen _ ->
            Keyboard.downs KeyDown
