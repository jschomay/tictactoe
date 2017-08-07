module Main exposing (..)

import TicTacToe
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tuple


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


type Msg
    = Move TicTacToe.Move
    | Replay


type alias Model =
    { board : TicTacToe.Board
    , player : TicTacToe.Player
    , score : ( Int, Int )
    }


model : Model
model =
    { board = TicTacToe.init
    , player = TicTacToe.X
    , score = ( 0, 0 )
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Move move ->
            let
                newBoard =
                    TicTacToe.play move model.board

                newScore =
                    case TicTacToe.state newBoard of
                        TicTacToe.Winner (TicTacToe.X) _ ->
                            Tuple.mapFirst ((+) 1) model.score

                        TicTacToe.Winner (TicTacToe.O) _ ->
                            Tuple.mapSecond ((+) 1) model.score

                        _ ->
                            model.score
            in
                { model
                    | board = newBoard
                    , score = newScore
                    , player =
                        if model.player == TicTacToe.X then
                            TicTacToe.O
                        else
                            TicTacToe.X
                }

        Replay ->
            { model | board = TicTacToe.init }


view : Model -> Html Msg
view model =
    let
        playerString player =
            if player == TicTacToe.X then
                "X"
            else
                "O"

        instructions =
            case TicTacToe.state model.board of
                TicTacToe.Winner player w ->
                    div [ class "instructions" ]
                        [ h3 [] [ text <| playerString player ++ " wins!" ]
                        , div [ class "play-again", onClick Replay ] [ text <| "Play again" ]
                        ]

                TicTacToe.CatsGame ->
                    div [ class "instructions" ]
                        [ h3 [] [ text "Cat's game!" ]
                        , div [ class "play-again", onClick Replay ] [ text <| "Play again" ]
                        ]

                _ ->
                    h3 [] [ text <| playerString model.player ++ "'s turn" ]
    in
        div [ class "layout" ]
            [ h1 [] [ text "Play Tic Tac Toe" ]
            , div [ class "score" ]
                [ span [] [ text <| "X: " ++ (toString <| Tuple.first model.score) ]
                , span [] [ text <| "O: " ++ (toString <| Tuple.second model.score) ]
                ]
            , instructions
            , TicTacToe.view model.player model.board Move
            ]
