module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Matrix exposing (..)
import Random.Pcg as Random
import Types exposing (..)
import Platform.Sub as Sub
import Keyboard exposing (KeyCode)
import TetrominoBag exposing (..)
import Window
import Time exposing (Time)
import AnimationFrame


type alias GameOnState =
    { matrix : Matrix
    , falling : Falling
    , level : Int
    , lines : Int
    , nextDrop : Time
    , time : Time
    , bag : Bag
    , score : Int
    , windowSize : Window.Size
    , settings : Settings
    }


type alias GameOverState =
    { score : Int
    , matrix : Matrix
    , seed : Random.Seed
    , windowSize : Window.Size
    , settings : Settings
    }


type alias HomeScreenState =
    { seed : Random.Seed
    , windowSize : Window.Size
    , settings : Settings
    }


type State
    = HomeScreen HomeScreenState
    | GameOn GameOnState
    | GameOver GameOverState


init : ( Int, ( Int, Int ) ) -> ( State, Cmd Msg )
init ( intSeed, ( width, height ) ) =
    ( HomeScreen <| HomeScreenState (Random.initialSeed intSeed) (Window.Size width height) defaultSettings, Cmd.none )


startNew : Settings -> Random.Seed -> Window.Size -> State
startNew settings seed windowSize =
    let
        ( tetromino, bag ) =
            spawn (initBag seed)

        matrix =
            Matrix.empty

        falling =
            newFalling matrix tetromino
    in
        GameOn <|
            { matrix = matrix
            , falling = falling
            , level = 1
            , lines = 0
            , nextDrop = 0.0
            , time = 0.0
            , bag = bag
            , score = 0
            , windowSize = windowSize
            , settings = settings
            }


type Msg
    = Tick Time
    | KeyDown KeyCode
    | Resize Window.Size
    | ChangeShadow Shadow


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
                            startNew state.settings state.seed state.windowSize

                        _ ->
                            HomeScreen state

                Resize size ->
                    HomeScreen { state | windowSize = size }

                Tick _ ->
                    HomeScreen state

                ChangeShadow shadow ->
                    HomeScreen { state | settings = setShadow shadow state.settings }

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
                    startNew state.settings state.seed state.windowSize

                _ ->
                    GameOver state

        Resize size ->
            GameOver { state | windowSize = size }

        Tick _ ->
            GameOver state

        ChangeShadow shadow ->
            GameOver { state | settings = setShadow shadow state.settings }


updateGameOn : Msg -> GameOnState -> State
updateGameOn msg state =
    case msg of
        Tick time ->
            let
                timedState =
                    { state | time = time }
            in
                if time >= state.nextDrop then
                    dropOne time timedState
                else
                    GameOn timedState

        KeyDown keyCode ->
            toKey keyCode
                |> Maybe.map (updateWithKey state)
                |> Maybe.withDefault (GameOn state)

        Resize size ->
            GameOn { state | windowSize = size }

        ChangeShadow shadow ->
            GameOn { state | settings = setShadow shadow state.settings }


dropOne : Time -> GameOnState -> State
dropOne now state =
    case moveDown state.matrix state.falling of
        Nothing ->
            dropEnd 0 state

        Just next ->
            let
                nextDrop =
                    now + ((dropInterval state.level) * Time.second)
            in
                GameOn { state | falling = next, nextDrop = nextDrop }


dropEnd : Int -> GameOnState -> State
dropEnd hardDropCount state =
    let
        ( count, matrix ) =
            state.matrix |> addBlocks state.falling |> removeFullLines

        ( tetromino, bag ) =
            spawn state.bag

        falling =
            newFalling state.matrix tetromino
    in
        if isValid state.matrix falling then
            let
                newLines =
                    state.lines + count
            in
                GameOn
                    { state
                        | matrix = matrix
                        , score = state.score + (getScore state.level count hardDropCount)
                        , lines = state.lines + count
                        , level = Basics.max (earnedLevel newLines) state.level
                        , falling = falling
                        , bag = bag
                    }
        else
            GameOver <| GameOverState state.score state.matrix state.bag.seed state.windowSize state.settings


getScore : Int -> Int -> Int -> Int
getScore level lines dropCount =
    let
        linesScore =
            case lines of
                1 ->
                    level * 100

                2 ->
                    level * 300

                3 ->
                    level * 500

                4 ->
                    level * 800

                _ ->
                    0
    in
        linesScore + dropCount


earnedLevel : Int -> Int
earnedLevel lines =
    1 + lines // 10


dropInterval : Int -> Time
dropInterval level =
    case level of
        1 ->
            0.5

        2 ->
            0.45

        3 ->
            0.4

        4 ->
            0.35

        5 ->
            0.3

        6 ->
            0.25

        7 ->
            0.2

        8 ->
            0.15

        9 ->
            0.1

        10 ->
            0.05

        _ ->
            0.05


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
                startNew state.settings state.bag.seed state.windowSize

            Left ->
                transform moveLeft

            Right ->
                transform moveRight

            Up ->
                transform rotateLeft

            Down ->
                transform moveDown

            Space ->
                case softDrop state.matrix state.falling of
                    Just ( count, next ) ->
                        dropEnd count { state | falling = next }

                    Nothing ->
                        GameOn state


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
        (viewSample state.settings (getCellSize state.windowSize))
        (div []
            [ h1 [] [ Html.text "Press F2 to start." ]
            , viewSettings state.settings
            ]
        )
        empty


viewGameOver : GameOverState -> Html Msg
viewGameOver state =
    viewGrid
        (viewMatrix state.settings (getCellSize state.windowSize) state.matrix Nothing)
        (div []
            [ h1 [] [ Html.text <| "Game over " ++ (toString state.score) ]
            , h2 [] [ Html.text "press F2 to start" ]
            , viewSettings state.settings
            ]
        )
        empty


viewGameOn : GameOnState -> Html Msg
viewGameOn state =
    viewGrid
        (viewMatrix state.settings (getCellSize state.windowSize) state.matrix (Just state.falling))
        (div []
            [ h1 [] [ text ("Score: " ++ (toString state.score)) ]
            , h1 [] [ text ("Level: " ++ (toString state.level)) ]
            , viewSettings state.settings
            ]
        )
        empty


viewSettings : Settings -> Html Msg
viewSettings settings =
    let
        shadowBtn title value =
            button
                [ classList [ ( "pure-button", True ), ( "pure-button-active", settings.shadow == value ) ]
                , onClick (ChangeShadow value)
                ]
                [ text title ]
    in
        div [ class "pure-form" ]
            [ fieldset []
                [ legend [] [ text "Shadow settings" ]
                , div [ class "pure-button-group", role "group", ariaLabel "shadow" ]
                    [ shadowBtn "None" None
                    , shadowBtn "Gray" Gray
                    , shadowBtn "Color" Color
                    ]
                ]
            ]


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
                Sub.batch [ always, AnimationFrame.times Tick ]

            GameOver _ ->
                always

            HomeScreen _ ->
                always
