module Main exposing (..)

import App exposing (..)
import Html exposing (programWithFlags)


main : Program ( Int, ( Int, Int ) ) State Msg
main =
    programWithFlags { view = view, init = init, update = update, subscriptions = subscriptions }
