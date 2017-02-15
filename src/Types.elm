module Types exposing (..)

import Dict exposing (Dict)
import Keyboard exposing (KeyCode)


type alias Color =
    String


type alias Offset =
    ( Int, Int )


addOffset : Offset -> Offset -> Offset
addOffset ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


subOffset : Offset -> Offset -> Offset
subOffset ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


type Rotation
    = R0
    | RR
    | R2
    | RL


type alias Blocks =
    Dict Offset Color


type Key
    = Left
    | Right
    | Up
    | Down
    | Space


toKey : KeyCode -> Maybe Key
toKey code =
    --|> Debug.log "KeyCode: "
    case code of
        32 ->
            Just Space

        37 ->
            Just Left

        38 ->
            Just Up

        39 ->
            Just Right

        40 ->
            Just Down

        _ ->
            Nothing
