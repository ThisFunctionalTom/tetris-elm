module Matrix exposing (..)

import Types exposing (..)
import Tetromino exposing (Tetromino)
import Dict exposing (Dict)
import Set


type alias Matrix =
    { width : Int
    , height : Int
    , blocks : Blocks
    }


fieldWidth : Int
fieldWidth =
    10


fieldHeight : Int
fieldHeight =
    22


empty : Matrix
empty =
    Matrix fieldWidth fieldHeight Dict.empty


addBlocks : Tetromino -> Matrix -> Matrix
addBlocks tetromino matrix =
    let
        tm : Dict Position Color
        tm =
            tetromino.positions
                |> Set.toList
                |> List.map (\pos -> ( add pos tetromino.offset, tetromino.color ))
                |> Dict.fromList

        blocks : Blocks
        blocks =
            Dict.union matrix.blocks tm
    in
        { matrix | blocks = blocks }
