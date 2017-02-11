module App exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String


type alias Model =
    { message : String
    , logo : String
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { message = "Your Elm App is working!", logo = path }, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


val : Int -> String
val value =
    toString value


vals : List Int -> String
vals values =
    values |> List.map toString |> String.join " "


playFieldBox : List Int
playFieldBox =
    [ 0, 0, 10, 22 ]
        |> List.map ((*) cellSize)


unitSize : Int
unitSize =
    30


type alias Block =
    { color : Color
    , cells : List Cell
    }


blocks : List Block
blocks =
    [ Block "red" [ ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 4, 1 ) ]
    , Block "magenta" [ ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 3, 2 ) ]
    , Block "yellow" [ ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 1, 2 ) ]
    , Block "cyan" [ ( 1, 1 ), ( 1, 2 ), ( 2, 1 ), ( 2, 2 ) ]
    , Block "blue" [ ( 2, 1 ), ( 3, 1 ), ( 1, 2 ), ( 2, 2 ) ]
    , Block "lightgray" [ ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 2, 2 ) ]
    , Block "lime" [ ( 1, 1 ), ( 2, 1 ), ( 2, 2 ), ( 3, 2 ) ]
    ]


view : Model -> Html Msg
view model =
    let
        w =
            toString <| 10 * unitSize

        h =
            toString <| 22 * unitSize
    in
        svg
            [ width w
            , height h
            , viewBox <| vals playFieldBox
            ]
            [ rect [ x "0", y "0", width w, height h, color "black" ] []
            , g []
                (blocks
                    |> List.indexedMap (\i b -> viewBlock ( 0, i * 3 ) b)
                )
            ]


viewBlock : ( Int, Int ) -> Block -> Svg msg
viewBlock ( x, y ) { color, cells } =
    cells
        |> List.map (\( cx, cy ) -> ( cx + x, cy + y ))
        |> List.map (viewCell color)
        |> g []


type alias Cell =
    ( Int, Int )


type alias Color =
    String


cellSize : Int
cellSize =
    20


viewCell : Color -> Cell -> Html msg
viewCell color ( cx, cy ) =
    rect
        [ x (toString <| cx * cellSize - 1)
        , y (toString <| cy * cellSize - 1)
        , width <| toString (cellSize - 2)
        , height <| toString (cellSize - 2)
        , fill color
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
