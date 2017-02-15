module Matrix exposing (..)

import Types exposing (..)
import Tetromino exposing (Tetromino)
import Dict exposing (Dict)


type alias Matrix =
    { width : Int
    , height : Int
    , blocks : Blocks
    }


type alias Falling =
    { tetromino : Tetromino
    , rotation : Rotation
    , offset : Offset
    }


newFalling : Matrix -> Tetromino -> Falling
newFalling matrix tetromino =
    Falling tetromino R0 (spawnOffset matrix tetromino)


spawnOffset : Matrix -> Tetromino -> Offset
spawnOffset matrix tetromino =
    let
        col =
            (matrix.width - tetromino.size) // 2
    in
        ( 0, col )


type ValidationResult
    = Valid
    | OutOfMatrix
    | BlockIntersection


validate : Matrix -> Falling -> ValidationResult
validate matrix falling =
    let
        intersects =
            matrix.blocks
                |> Dict.keys
                |> List.any (\pos -> blocks falling |> Dict.member pos)

        inMatrix =
            blocks falling
                |> Dict.filter (\( col, row ) _ -> row >= matrix.height || col < 0 || col >= matrix.width)
                |> Dict.isEmpty
    in
        if not inMatrix then
            OutOfMatrix
        else if intersects then
            BlockIntersection
        else
            Valid


isValid : Matrix -> Falling -> Bool
isValid matrix falling =
    let
        intersects =
            matrix.blocks
                |> Dict.keys
                |> List.any (\pos -> blocks falling |> Dict.member pos)

        inMatrix =
            blocks falling
                |> Dict.filter (\( row, col ) _ -> row >= matrix.height || col < 0 || col >= matrix.width)
                |> Dict.isEmpty
    in
        inMatrix && not intersects


type alias Transformation =
    Falling -> Falling


blocks : Falling -> Blocks
blocks falling =
    Tetromino.blocks falling.offset falling.rotation falling.tetromino


rotateLeft : Matrix -> Falling -> Maybe Falling
rotateLeft matrix falling =
    let
        rotation =
            case falling.rotation of
                R0 ->
                    RL

                RL ->
                    R2

                R2 ->
                    RR

                RR ->
                    R0

        next =
            { falling | rotation = rotation }
    in
        if isValid matrix next then
            Just next
        else
            Nothing


rotateRight : Matrix -> Falling -> Maybe Falling
rotateRight matrix falling =
    let
        rotation =
            case falling.rotation of
                R0 ->
                    RR

                RR ->
                    R2

                R2 ->
                    RL

                RL ->
                    R0

        next =
            { falling | rotation = rotation }
    in
        if isValid matrix next then
            Just next
        else
            Nothing


moveLeft : Matrix -> Falling -> Maybe Falling
moveLeft matrix falling =
    let
        next =
            { falling | offset = addOffset ( 0, -1 ) falling.offset }
    in
        if isValid matrix next then
            Just next
        else
            Nothing


moveRight : Matrix -> Falling -> Maybe Falling
moveRight matrix falling =
    let
        next =
            { falling | offset = addOffset ( 0, 1 ) falling.offset }
    in
        if isValid matrix next then
            Just next
        else
            Nothing


moveDown : Matrix -> Falling -> Maybe Falling
moveDown matrix falling =
    let
        next =
            { falling | offset = addOffset ( 1, 0 ) falling.offset }
    in
        if isValid matrix next then
            Just next
        else
            Nothing


fieldWidth : Int
fieldWidth =
    10


fieldHeight : Int
fieldHeight =
    22


empty : Matrix
empty =
    Matrix fieldWidth fieldHeight Dict.empty


addBlocks : Falling -> Matrix -> Matrix
addBlocks falling matrix =
    let
        tblocks =
            Dict.union matrix.blocks (blocks falling)
    in
        { matrix | blocks = tblocks }
