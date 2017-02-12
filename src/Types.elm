module Types exposing (..)

import Dict exposing (Dict)
import Keyboard exposing (KeyCode)


type alias Color =
    String


type alias Position =
    ( Int, Int )


add : Position -> Position -> Position
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


type alias Blocks =
    Dict Position Color


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
