module MatrixTests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Random.Pcg as Random
import Shrink
import Types exposing (Offset, Blocks)
import Set exposing (Set)
import Tetromino
import Types exposing (Color)
import Dict exposing (Dict)
import Matrix exposing (Matrix)


colors : List Color
colors =
    Tetromino.tetrominos
        |> List.map .color


cellGen : Random.Generator (Maybe Color)
cellGen =
    let
        justColor =
            colors |> List.map Just
    in
        Random.sample (List.append justColor [ Nothing, Nothing, Nothing ])
            |> Random.map (Maybe.withDefault Nothing)


rowGen : Random.Generator (List (Maybe Color))
rowGen =
    Random.list 2 cellGen


type alias ColorGrid =
    List (List (Maybe Color))


blocksGen : Random.Generator ColorGrid
blocksGen =
    Random.list 2 rowGen


colorGrid : Fuzz.Fuzzer ColorGrid
colorGrid =
    Fuzz.custom blocksGen Shrink.noShrink


all : List Test
all =
    [ fuzz colorGrid "Converting toList and back toBlocks is identity" <|
        \cg -> cg |> Matrix.fromList |> Matrix.toList |> Expect.equal cg
    , test "Simple ColorGrid to Matrix" <|
        \() ->
            let
                matrix =
                    [ [ Just "red", Just "blue" ], [ Just "yellow", Just "green" ] ]
                        |> Matrix.fromList

                blocks =
                    matrix.blocks

                at pos value blocks =
                    blocks |> Dict.get pos |> Expect.equal (Just value)
            in
                Expect.all
                    [ at ( 0, 0 ) "red"
                    , at ( 0, 1 ) "blue"
                    , at ( 1, 0 ) "yellow"
                    , at ( 1, 1 ) "green"
                    ]
                    blocks
    , test "Simple Matrix to Grid" <|
        \() ->
            let
                grid : Matrix
                grid =
                    [ ( ( 0, 0 ), "red" )
                    , ( ( 0, 1 ), "blue" )
                    , ( ( 1, 0 ), "yellow" )
                    , ( ( 1, 1 ), "green" )
                    ]
                        |> Dict.fromList
                        |> Matrix 2 2
            in
                grid
                    |> Matrix.toList
                    |> Expect.equal [ [ Just "red", Just "blue" ], [ Just "yellow", Just "green" ] ]
    , test "Range + Map" <|
        \() ->
            List.range 0 1
                |> List.map (\row -> List.range 0 1 |> List.map (\col -> ( row, col )))
                |> Expect.equal [ [ ( 0, 0 ), ( 0, 1 ) ], [ ( 1, 0 ), ( 1, 1 ) ] ]
    ]
