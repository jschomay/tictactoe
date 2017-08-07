module Tests exposing (all)

import ElmTest.Extra exposing (..)
import Expect exposing (..)
import Fuzz exposing (..)
import TicTacToe exposing (..)


all : Test
all =
    describe "TicTacToe"
        [ test "accessing when empty" <|
            \_ ->
                TicTacToe.init
                    |> TicTacToe.at ( Row1, Col2 )
                    |> Expect.equal Nothing
        , test "accessing when set" <|
            \_ ->
                TicTacToe.init
                    |> TicTacToe.play (move X Row1 Col2)
                    |> TicTacToe.at ( Row1, Col2 )
                    |> Expect.equal (Just X)
        , fuzz2 (list <| randomMove X) (randomMove X) "available" <|
            \moves move2 ->
                TicTacToe.init
                    |> (\board -> List.foldl TicTacToe.play board moves)
                    |> TicTacToe.available move2
                    |> Expect.equal (not <| List.member move2 moves)
        , test "cats game" <|
            \_ ->
                TicTacToe.init
                    |> TicTacToe.play (move X Row1 Col1)
                    |> TicTacToe.play (move X Row1 Col2)
                    |> TicTacToe.play (move O Row1 Col3)
                    |> TicTacToe.play (move O Row2 Col1)
                    |> TicTacToe.play (move O Row2 Col2)
                    |> TicTacToe.play (move X Row2 Col3)
                    |> TicTacToe.play (move X Row3 Col1)
                    |> TicTacToe.play (move O Row3 Col2)
                    |> TicTacToe.play (move X Row3 Col3)
                    |> TicTacToe.state
                    |> Expect.equal CatsGame
        , test "still playing" <|
            \_ ->
                TicTacToe.init
                    |> TicTacToe.state
                    |> Expect.equal Playing
        , test "winner" <|
            \_ ->
                TicTacToe.init
                    |> TicTacToe.play (move X Row1 Col1)
                    |> TicTacToe.play (move X Row2 Col1)
                    |> TicTacToe.play (move X Row3 Col1)
                    |> TicTacToe.state
                    |> Expect.equal (Winner X C1)
        ]


randomMove : Player -> Fuzzer Move
randomMove player =
    let
        row =
            oneOf [ constant Row1, constant Row2, constant Row3 ]

        col =
            oneOf [ constant Col1, constant Col2, constant Col3 ]
    in
        Fuzz.map2 (move player) row col
