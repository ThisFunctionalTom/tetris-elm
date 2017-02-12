module Tetromino exposing (..)

import String
import Set exposing (Set)
import Dict
import Types exposing (..)


type alias Tetromino =
    { gridSize : Int
    , color : Color
    , positions : Set Position
    , offset : Position
    }


tetromino : String -> List String -> Tetromino
tetromino color lines =
    let
        gridSize : Int
        gridSize =
            List.length lines

        blocks : List String -> Set Position
        blocks lines =
            lines
                |> List.indexedMap (\row line -> getRow row line)
                |> List.concat
                |> Set.fromList

        getRow : Int -> String -> List Position
        getRow row line =
            line
                |> String.toList
                |> List.indexedMap (\col char -> getPos row col char)
                |> List.filterMap identity

        getPos : Int -> Int -> Char -> Maybe Position
        getPos row col char =
            if char == 'X' then
                Just ( col, row )
            else
                Nothing
    in
        Tetromino gridSize color (blocks lines) ( 2, 2 )


rotatePositionLeft : Int -> Position -> Position
rotatePositionLeft size ( row, col ) =
    ( size - 1 - col, row )


rotatePositionRight : Int -> Position -> Position
rotatePositionRight size ( row, col ) =
    ( col, size - 1 - row )


rotateLeft : Tetromino -> Tetromino
rotateLeft =
    mapBlocks rotatePositionLeft


rotateRight : Tetromino -> Tetromino
rotateRight =
    mapBlocks rotatePositionRight


moveLeft : Tetromino -> Tetromino
moveLeft tetromino =
    { tetromino | offset = add ( -1, 0 ) tetromino.offset }


moveRight : Tetromino -> Tetromino
moveRight tetromino =
    { tetromino | offset = add ( 1, 0 ) tetromino.offset }


moveDown : Tetromino -> Tetromino
moveDown tetromino =
    { tetromino | offset = add ( 0, 1 ) tetromino.offset }


blocks : Tetromino -> Blocks
blocks { positions, offset, color } =
    positions
        |> Set.map (\pos -> ( add pos offset, color ))
        |> Set.toList
        |> Dict.fromList


mapBlocks : (Int -> Position -> Position) -> Tetromino -> Tetromino
mapBlocks f tetromino =
    let
        positions =
            tetromino.positions
                |> Set.map (\pos -> f tetromino.gridSize pos)
    in
        { tetromino | positions = positions }


tetrominoI : Tetromino
tetrominoI =
    tetromino "cyan"
        [ "    "
        , "XXXX"
        , "    "
        , "    "
        ]


tetrominoJ : Tetromino
tetrominoJ =
    tetromino "blue"
        [ "X  "
        , "XXX"
        , "   "
        ]


tetrominoL : Tetromino
tetrominoL =
    tetromino "orange"
        [ "  X"
        , "XXX"
        , "   "
        ]


tetrominoO : Tetromino
tetrominoO =
    tetromino "yellow"
        [ "XX"
        , "XX"
        ]


tetrominoS : Tetromino
tetrominoS =
    tetromino "green"
        [ " XX"
        , "XX "
        , "   "
        ]


tetrominoT : Tetromino
tetrominoT =
    tetromino "purple"
        [ " X "
        , "XXX"
        , "   "
        ]


tetrominoZ : Tetromino
tetrominoZ =
    tetromino "red"
        [ "XX "
        , " XX"
        , "   "
        ]


tetrominos : List Tetromino
tetrominos =
    [ tetrominoI
    , tetrominoJ
    , tetrominoL
    , tetrominoO
    , tetrominoS
    , tetrominoT
    , tetrominoZ
    ]
