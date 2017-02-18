module TetrominoBag exposing (..)

import Random.Pcg as Random
import Tetromino exposing (Tetromino, tetrominos)


type alias Bag =
    { seed : Random.Seed
    , tetrominos : List Tetromino
    }


initBag : Random.Seed -> Bag
initBag seed =
    Bag seed tetrominos


spawn : Bag -> ( Tetromino, Bag )
spawn { seed, tetrominos } =
    case tetrominos of
        [] ->
            ( Tetromino.tetrominoT, Bag seed Tetromino.tetrominos )

        lastOne :: [] ->
            ( lastOne, Bag seed Tetromino.tetrominos )

        default :: rest ->
            let
                generator =
                    Random.sample tetrominos |> Random.map (Maybe.withDefault default)

                ( chosen, nextSeed ) =
                    Random.step generator seed
            in
                ( chosen, Bag nextSeed (List.filter ((/=) chosen) tetrominos) )
