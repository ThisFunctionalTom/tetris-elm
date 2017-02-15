module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import TetrominoTests


all : Test
all =
    describe "Tetromino tests"
        TetrominoTests.all
