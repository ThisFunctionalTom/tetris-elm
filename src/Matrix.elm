module Matrix exposing (..)

import Types exposing (..)
import Tetromino exposing (Tetromino)
import Dict exposing (Dict)


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
        blocks =
            Dict.union matrix.blocks (Tetromino.blocks tetromino)
    in
        { matrix | blocks = blocks }
