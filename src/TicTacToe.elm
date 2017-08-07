module TicTacToe
    exposing
        ( Board
        , Player(..)
        , State(..)
        , Win(..)
        , Row(..)
        , Col(..)
        , Move
        , init
        , move
        , at
        , available
        , play
        , state
        , view
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)


type Board
    = Board (Array (Maybe Player))


type Row
    = Row1
    | Row2
    | Row3


type Col
    = Col1
    | Col2
    | Col3


type Player
    = X
    | O


type Move
    = Move Player ( Row, Col )


type State
    = Playing
    | Winner Player Win
    | CatsGame


type Win
    = R1
    | R2
    | R3
    | C1
    | C2
    | C3
    | D1
    | D2


init : Board
init =
    Board <| Array.repeat 9 Nothing


move : Player -> Row -> Col -> Move
move player row col =
    Move player ( row, col )


at : ( Row, Col ) -> Board -> Maybe Player
at pos (Board board) =
    Array.get (index pos) board
        |> Maybe.andThen identity


available : Move -> Board -> Bool
available (Move _ pos) board =
    at pos board == Nothing


{-| Commit a move
-}
play : Move -> Board -> Board
play (Move player pos) (Board board) =
    Board <| Array.set (index pos) (Just player) board


{-| Assumes only one possible winner, finds only the first winner in a board
-}
state : Board -> State
state ((Board board_) as board) =
    let
        winners =
            [ ( R1, [ ( Row1, Col1 ), ( Row1, Col2 ), ( Row1, Col3 ) ] )
            , ( R2, [ ( Row2, Col1 ), ( Row2, Col2 ), ( Row2, Col3 ) ] )
            , ( R3, [ ( Row3, Col1 ), ( Row3, Col2 ), ( Row3, Col3 ) ] )
            , ( C1, [ ( Row1, Col1 ), ( Row2, Col1 ), ( Row3, Col1 ) ] )
            , ( C2, [ ( Row1, Col2 ), ( Row2, Col2 ), ( Row3, Col2 ) ] )
            , ( C3, [ ( Row1, Col3 ), ( Row2, Col3 ), ( Row3, Col3 ) ] )
            , ( D1, [ ( Row1, Col1 ), ( Row2, Col2 ), ( Row3, Col3 ) ] )
            , ( D2, [ ( Row3, Col1 ), ( Row2, Col2 ), ( Row1, Col3 ) ] )
            ]

        winner_ ( w, l ) =
            if List.all (\pos -> at pos board == Just X) l then
                Just ( w, X )
            else if List.all (\pos -> at pos board == Just O) l then
                Just ( w, O )
            else
                Nothing
    in
        case List.filterMap winner_ winners |> List.head of
            Just ( w, X ) ->
                Winner X w

            Just ( w, O ) ->
                Winner O w

            Nothing ->
                if List.all ((/=) Nothing) <| Array.toList board_ then
                    CatsGame
                else
                    Playing


index : ( Row, Col ) -> Int
index ( row, col ) =
    let
        rowIndex =
            case row of
                Row1 ->
                    0

                Row2 ->
                    1

                Row3 ->
                    2

        colIndex =
            case col of
                Col1 ->
                    0

                Col2 ->
                    1

                Col3 ->
                    2
    in
        rowIndex * 3 + colIndex



-- View


view : Player -> Board -> (Move -> msg) -> Html msg
view player board msg =
    let
        boardClasses =
            classList
                [ ( "board", True )
                , ( "board--x-move", player == X )
                , ( "board--o-move", player == O )
                ]

        cellClasses value =
            classList
                [ ( "board__cell", True )
                , ( "board__cell--x", value == Just X )
                , ( "board__cell--o", value == Just O )
                , ( "board__cell--available", state board == Playing && value == Nothing )
                ]

        cellAttrs pos =
            let
                move_ =
                    uncurry (move player) pos
            in
                [ cellClasses <| at pos board ]
                    ++ if state board == Playing && available move_ board then
                        [ onClick <| msg <| move_ ]
                       else
                        []

        winningLine =
            case state board of
                Winner _ w ->
                    [ span [ class <| "board__winning-line board__winning-line" ++ "--" ++ toString w ] [] ]

                _ ->
                    []
    in
        div [ class "TicTacToe" ] <|
            winningLine
                ++ [ div [ boardClasses ]
                        [ div (cellAttrs ( Row1, Col1 )) []
                        , div (cellAttrs ( Row1, Col2 )) []
                        , div (cellAttrs ( Row1, Col3 )) []
                        , div (cellAttrs ( Row2, Col1 )) []
                        , div (cellAttrs ( Row2, Col2 )) []
                        , div (cellAttrs ( Row2, Col3 )) []
                        , div (cellAttrs ( Row3, Col1 )) []
                        , div (cellAttrs ( Row3, Col2 )) []
                        , div (cellAttrs ( Row3, Col3 )) []
                        ]
                   ]
