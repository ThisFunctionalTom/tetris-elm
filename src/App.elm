module App exposing (..)

import Html exposing (Html, h1, h2, div, text)
import Html.Attributes exposing (class)
import Time exposing (Time)
import Matrix exposing (..)
import Random.Pcg as Random
import Types exposing (..)
import Platform.Sub as Sub
import Keyboard exposing (KeyCode)
import TetrominoBag exposing (..)
import Window


type alias GameOnState =
    { matrix : Matrix
    , falling : Falling
    , bag : Bag
    , score : Int
    , windowSize : Window.Size
    }


type alias GameOverState =
    { score : Int
    , matrix : Matrix
    , seed : Random.Seed
    , windowSize : Window.Size
    }


type alias HomeScreenState =
    { seed : Random.Seed
    , windowSize : Window.Size
    }


type State
    = HomeScreen HomeScreenState
    | GameOn GameOnState
    | GameOver GameOverState


init : ( Int, ( Int, Int ) ) -> ( State, Cmd Msg )
init ( intSeed, ( width, height ) ) =
    ( HomeScreen <| HomeScreenState (Random.initialSeed intSeed) (Window.Size width height), Cmd.none )


startNew : Random.Seed -> Window.Size -> State
startNew seed windowSize =
    let
        ( tetromino, bag ) =
            spawn (initBag seed)

        matrix =
            Matrix.empty

        falling =
            newFalling matrix tetromino
    in
        GameOn <| GameOnState matrix falling bag 0 windowSize


type Msg
    = Tick Time
    | KeyDown KeyCode
    | Resize Window.Size


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    ( simpleUpdate msg state, Cmd.none )


simpleUpdate : Msg -> State -> State
simpleUpdate msg state =
    case state of
        HomeScreen state ->
            case msg of
                KeyDown keyCode ->
                    case toKey keyCode of
                        Just F2 ->
                            startNew state.seed state.windowSize

                        _ ->
                            HomeScreen state

                Resize size ->
                    HomeScreen { state | windowSize = size }

                Tick _ ->
                    HomeScreen state

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
                    startNew state.seed state.windowSize

                _ ->
                    GameOver state

        Resize size ->
            GameOver { state | windowSize = size }

        Tick _ ->
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
                            GameOver <| GameOverState state.score state.matrix state.bag.seed state.windowSize

                Just next ->
                    GameOn { state | falling = next }

        KeyDown keyCode ->
            toKey keyCode
                |> Maybe.map (updateWithKey state)
                |> Maybe.withDefault (GameOn state)

        Resize size ->
            GameOn { state | windowSize = size }


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
                startNew state.bag.seed state.windowSize

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


view : State -> Html Msg
view state =
    case state of
        HomeScreen hss ->
            viewHomeScreen hss

        GameOver gos ->
            viewGameOver gos

        GameOn gos ->
            viewGameOn gos


getCellSize : Window.Size -> Int
getCellSize windowSize =
    windowSize.height // 22


viewGrid : Html msg -> Html msg -> Html msg -> Html msg
viewGrid left middle right =
    div [ class "pure-g" ]
        [ div [ class "pure-u-1-3" ] [ left ]
        , div [ class "pure-u-1-3" ] [ middle ]
        , div [ class "pure-u-1-3" ] [ right ]
        ]


empty : Html Msg
empty =
    text ""


viewHomeScreen : HomeScreenState -> Html Msg
viewHomeScreen state =
    viewGrid
        (viewSample (getCellSize state.windowSize))
        (h1 [] [ Html.text "Press F2 to start." ])
        empty


viewGameOver : GameOverState -> Html Msg
viewGameOver state =
    viewGrid
        (viewMatrix (getCellSize state.windowSize) state.matrix [])
        (div []
            [ h1 [] [ Html.text <| "Game over " ++ (toString state.score) ]
            , h2 [] [ Html.text "press F2 to start" ]
            ]
        )
        empty


viewGameOn : GameOnState -> Html Msg
viewGameOn state =
    viewGrid
        (viewMatrix (getCellSize state.windowSize) state.matrix [ state.falling ])
        (h1 [] [ Html.text (toString state.score) ])
        empty


subscriptions : State -> Sub Msg
subscriptions state =
    let
        always =
            Sub.batch
                [ Keyboard.downs KeyDown
                , Window.resizes Resize
                ]
    in
        case state of
            GameOn _ ->
                Sub.batch [ always, Time.every (250 * Time.millisecond) (\t -> Tick t) ]

            GameOver _ ->
                always

            HomeScreen _ ->
                always
