module Main exposing (..)

import App exposing (..)
import Html exposing (program)


main : Program Never State Msg
main =
    program { view = view, init = init, update = update, subscriptions = subscriptions }
