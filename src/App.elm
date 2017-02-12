module App exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String
import Dict
import Set
import Time exposing (Time)
import Tetromino exposing (..)
import Matrix exposing (..)
import Random.Pcg as Random
import Random.Pcg.Interop exposing (fission)
import Types exposing (..)
import Platform.Sub as Sub
import Keyboard exposing (KeyCode)


type alias State =
    { matrix : Matrix
    , falling : Maybe Tetromino
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
                    let
                        next =
                            moveDown falling
                    in
                        if isValid state.matrix next then
                            ( { state | falling = Just next }, Cmd.none )
                        else
                            ( { state | matrix = state.matrix |> addBlocks falling }, nextFalling )

        NewTetromino maybeTetromino ->
            ( { state | falling = maybeTetromino }, Cmd.none )

        KeyDown keyCode ->
            ( keyCode |> toKey |> Maybe.map (updateWithKey state) |> Maybe.withDefault state, Cmd.none )


updateFalling : (Tetromino -> Tetromino) -> State -> State
updateFalling f state =
    case state.falling of
        Nothing ->
            state

        Just falling ->
            let
                next =
                    f falling
            in
                if isValid state.matrix next then
                    { state | falling = Just next }
                else
                    state


isValid : Matrix -> Tetromino -> Bool
isValid matrix tetromino =
    let
        intersects =
            matrix.blocks
                |> Dict.keys
                |> List.any (\pos -> Tetromino.blocks tetromino |> Dict.member pos)

        inMatrix =
            Tetromino.blocks tetromino
                |> Dict.filter (\( col, row ) _ -> row >= matrix.height || col < 0 || col >= matrix.width)
                |> Dict.isEmpty
    in
        not intersects && inMatrix


updateWithKey : State -> Key -> State
updateWithKey state key =
    case key of
        Left ->
            state
                |> updateFalling moveLeft

        Right ->
            state
                |> updateFalling moveRight

        Up ->
            state |> updateFalling rotateRight

        Down ->
            state |> updateFalling moveDown

        Space ->
            state |> updateFalling (softDrop state.matrix)


softDrop : Matrix -> Tetromino -> Tetromino
softDrop matrix valid =
    let
        next =
            moveDown valid
    in
        if isValid matrix next then
            softDrop matrix next
        else
            valid


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


viewFalling : Tetromino -> Svg msg
viewFalling tetromino =
    Tetromino.blocks tetromino
        |> viewBlocks


viewBlocks : Blocks -> Svg msg
viewBlocks blocks =
    g []
        (blocks
            |> Dict.toList
            |> List.map viewBlock
        )


viewBlock : ( Position, Color ) -> Svg msg
viewBlock ( ( col, row ), color ) =
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
