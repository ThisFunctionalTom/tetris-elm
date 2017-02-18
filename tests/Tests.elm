module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import TetrominoTests
import MatrixTests


all : Test
all =
    describe "All"
        [ describe "Tetromino tests" TetrominoTests.all
        , describe "Matrix tests" MatrixTests.all
        ]
