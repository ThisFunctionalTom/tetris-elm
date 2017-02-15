module TetrominoTests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Shrink
import Random.Pcg as Random
import Tetromino exposing (..)
import Types exposing (Position)
import Set exposing (Set)


tetromino : Fuzzer Tetromino
tetromino =
    let
        randomTetromino =
            Random.sample tetrominos
                |> Random.map (Maybe.withDefault tetrominoI)
    in
        Fuzz.custom
            randomTetromino
            Shrink.noShrink


all : List Test
all =
    [ fuzz tetromino "Tetromino has exactly four blocks" <|
        \t ->
            t.positions
                |> Set.size
                |> Expect.equal 4
    , fuzz tetromino "All blocks are within the tetromino grid size" <|
        \t ->
            t.positions
                |> Set.filter
                    (\( col, row ) -> col >= 0 && col < t.gridSize && row >= 0 && row < t.gridSize)
                |> Set.size
                |> Expect.equal 4
    , fuzz tetromino "All blocks have a orthogonal neighbour" <|
        \t ->
            let
                neighbours : Position -> Set Position
                neighbours ( col, row ) =
                    Set.fromList [ ( col + 1, row ), ( col - 1, row ), ( col, row + 1 ), ( col, row - 1 ) ]

                hasNeighbour : Set Position -> Position -> Bool
                hasNeighbour positions ( col, row ) =
                    neighbours ( col, row )
                        |> Set.intersect t.positions
                        |> Set.isEmpty
                        |> not
            in
                t.positions
                    |> Set.filter (hasNeighbour t.positions)
                    |> Set.size
                    |> Expect.equal 4
    ]
