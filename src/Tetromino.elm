module Tetromino exposing (..)

import String
import Set exposing (Set)
import Types exposing (..)
import Dict


type alias Kicks =
    { r0 : List Offset
    , rR : List Offset
    , r2 : List Offset
    , rL : List Offset
    }


type alias Tetromino =
    { size : Int
    , color : Color
    , minos : Set Offset
    , kicks : Kicks
    }


tetromino : String -> List String -> Kicks -> Tetromino
tetromino color lines kicks =
    let
        size : Int
        size =
            List.length lines

        minos : List String -> Set Offset
        minos lines =
            lines
                |> List.indexedMap (\row line -> getRow row line)
                |> List.concat
                |> Set.fromList

        getRow : Int -> String -> List Offset
        getRow row line =
            line
                |> String.toList
                |> List.indexedMap (\col char -> getPos row col char)
                |> List.filterMap identity

        getPos : Int -> Int -> Char -> Maybe Offset
        getPos row col char =
            if char == 'X' then
                Just ( row, col )
            else
                Nothing
    in
        Tetromino size color (minos lines) kicks


blocks : Offset -> Rotation -> Tetromino -> Blocks
blocks offset rotation { size, minos, color } =
    minos
        |> rotate rotation size
        |> Set.map (addOffset offset)
        |> Set.toList
        |> List.map (\offset -> ( offset, color ))
        |> Dict.fromList


rotate : Rotation -> Int -> Set Offset -> Set Offset
rotate rotation size minos =
    case rotation of
        R0 ->
            minos

        RL ->
            minos |> Set.map (rotateMinosLeft size)

        RR ->
            minos |> Set.map (rotateMinosRight size)

        R2 ->
            minos |> Set.map (rotateMinosTwice size)


rotateMinosLeft : Int -> Offset -> Offset
rotateMinosLeft size ( row, col ) =
    ( size - 1 - col, row )


rotateMinosRight : Int -> Offset -> Offset
rotateMinosRight size ( row, col ) =
    ( col, size - 1 - row )


rotateMinosTwice : Int -> Offset -> Offset
rotateMinosTwice size ( row, col ) =
    ( size - row, size - col )


tetrominoI : Tetromino
tetrominoI =
    tetromino "cyan"
        [ "    "
        , "XXXX"
        , "    "
        , "    "
        ]
        kicks4


tetrominoJ : Tetromino
tetrominoJ =
    tetromino "blue"
        [ "X  "
        , "XXX"
        , "   "
        ]
        kicks3


tetrominoL : Tetromino
tetrominoL =
    tetromino "orange"
        [ "  X"
        , "XXX"
        , "   "
        ]
        kicks3


tetrominoO : Tetromino
tetrominoO =
    tetromino "yellow"
        [ "XX"
        , "XX"
        ]
        kicks2


tetrominoS : Tetromino
tetrominoS =
    tetromino "green"
        [ " XX"
        , "XX "
        , "   "
        ]
        kicks3


tetrominoT : Tetromino
tetrominoT =
    tetromino "purple"
        [ " X "
        , "XXX"
        , "   "
        ]
        kicks3


tetrominoZ : Tetromino
tetrominoZ =
    tetromino "red"
        [ "XX "
        , " XX"
        , "   "
        ]
        kicks3


kicks3 : Kicks
kicks3 =
    Kicks
        [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ]
        [ ( 0, 0 ), ( 1, 0 ), ( 1, -1 ), ( 0, 2 ), ( 1, 2 ) ]
        [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ]
        [ ( 0, 0 ), ( -1, 0 ), ( -1, -1 ), ( 0, 2 ), ( -1, 2 ) ]


kicks4 : Kicks
kicks4 =
    Kicks
        [ ( 0, 0 ), ( -1, 0 ), ( 2, 0 ), ( -1, 0 ), ( 2, 0 ) ]
        [ ( -1, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 1 ), ( 0, -2 ) ]
        [ ( -1, 1 ), ( 1, 1 ), ( -2, 1 ), ( 1, 0 ), ( -2, 0 ) ]
        [ ( 0, 1 ), ( 0, 1 ), ( 0, 1 ), ( 0, -1 ), ( 0, 2 ) ]


kicks2 : Kicks
kicks2 =
    Kicks
        [ ( 0, 0 ) ]
        [ ( 0, -1 ) ]
        [ ( -1, -1 ) ]
        [ ( -1, 0 ) ]


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
