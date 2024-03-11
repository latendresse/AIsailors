--
-- Copyright 2023, Mario Latendresse, All rights reserved.
--
-- AI Sailors game.
--

module Sailors exposing (main)

import Core     exposing (..)
import Board    exposing (..)
import Sailing  exposing (..)
import Computer exposing (searchAlphaBeta)

import Browser
import Html        as H  exposing (..)
import Html.Attributes   exposing (style)
import Element     as E

import List   as L  exposing (..)
import String as S
import Maybe        exposing (withDefault)

import Html.Events  exposing (onClick)
import Html.Events.Extra.Mouse as Mouse

import Browser.Events     exposing (onMouseUp, onMouseDown)
import Json.Decode as D

import Time

-- Possible colors: black, silver, gray, white, marron, red,purple, fuchsia, green, lime, olive,
-- yellow, navy, blue, teal, aqua. See https://developer.mozilla.org/en-US/docs/Web/CSS/named-color/
--

initialModel : Model
initialModel =
    { playerTurn        = PlayerA     -- ^ PlayerA always has the first move.
    , sumSailDistancesA = 0
    , sumSailDistancesB = 0
    , sumBackMovesA     = 0
    , sumBackMovesB     = 0
    , sailsPositionsA   = L.map (\i -> ((i, 6), ToLeft)) <| range 1 5
    , sailsPositionsB   = L.map (\j -> ((j, 6), ToUp))   <| range 1 5
    , value             = 0                  -- The current value of this model.
    , estValue          = 0                  -- The estimated future value of this model.
    , msg               = ""
    , computerPlayer    = PlayerA
    , computerState     = NotThinking
    , lastMove          = (0, 0)
    , state             = InitialScreen
    , pState            = InitialScreen
    , searchDepth       = 12            -- ^ The default strength level is strong.
    }

init : () -> (Model, Cmd Msg)
init flags = (initialModel, Cmd.none)

-- | Listen to clicks on the board and when it is the turn of the computer to play.
subscriptions : Model -> Sub Msg
subscriptions model =
   if model.computerPlayer == model.playerTurn && winner model == NoWinner && model.state == Started
   then case model.computerState of
          NotThinking   -> Time.every 100 (\time -> ComputerThink)
          Thinking      -> Time.every 100 (\time -> ComputerSearch)
   else
       Sub.batch
           [onMouseUp (D.map (.offsetPos >> MouseClick) Mouse.eventDecoder)]

-- | Anakyze the position of a click on the screen. It typically is a click on a sail.
-- It could be an erroneous click outside the board or on an opponent sail.
analyzeClick: (Float, Float) -> Model -> Model
analyzeClick (x, y) model =
  let eiSailPos = fromXYtoSailPos (x, y) model
  in case eiSailPos of
       Err errMsg  -> {model | msg = errMsg}
       Ok sailPos -> let newModel = moveSailFrom model sailPos
                     in {newModel | msg = ""}

-- | The view display the entire board with sails with a selection to make
-- before starting the game.
view : Model -> Browser.Document Msg
view model =
  let iToS   = S.fromInt
      header = [div [ style "display"         "flex"
                    , style "justify-content" "center"
                    , style "align-items"     "center"
                    , style "height"          "60px"
                    , style "width"           <| (iToS <| bv.widthBoard + 3*bv.x0)++"px"
                    ]
                    [ h1 []  [text "Sailors" ]]]
      styleBackground1 = [ style "display"          "flex"
                         , style "height"           "450px"
                         , style "width"            "450px"
                         , style "text-align"       "center"
                         , style "align-items"      "center"
                         , style "justify-content"  "center"
                         , style "background-color" "white"
                         , style "position"         "absolute"
                         , style "top"              <| (iToS bv.y0)++"px"
                         , style "opacity"          "0.95"]

      styleBackground2 = [ style "display"          "flex"
                         , style "height"           "50px"
                         , style "width"            "450px"
                         , style "text-align"       "center"
                         , style "align-items"      "center"
                         , style "justify-content"  "center"
                         , style "background-color" "black"
                         , style "position"         "absolute"
                         , style "top"              <| (iToS bv.y0)++"px"
                         , style "opacity"          "0.95"]

      instructions1 = """You are directing a team of five racing sail
                      boats against your opponent, the Computer team with the same number of boats.
                      To play a move, you select one sail boat to go forward. The speed of each sail
                      boat, at 1, 2, or 3 steps, is given by the number of dots of the last
                      departing dock. Your objective is to get four of your sail boats to the
                      opposite dock and come back to the starting dock before your opponent does.
                      Your opponent will try to stop you by going over your sail boats, which forces
                      them back to their last departing dock. Likewise, you can hinder your opponent
                      sail boats.

                      P.S.: If four sail boats made it back, and that team was the first to play,
                      the opponent may draw the game with one ultimate move to complete four sail
                      boats back. Yes, a draw is possible at Sailors!
                      """

      showInstructions = [ div styleBackground1
                               [ p [ style "text-align" "justify"
                                   , style "width"      "80%"]
                                     [text instructions1]]
                         ]
      -- Stack buttons to click with a background to cover the board or bring attention.
      stackButtons styleBackground question buttons =
          [ div
            styleBackground
            [ div [ style "opacity" "1.0"
                  , style "width"   "350px"
                  , style "vertical-align"   "center"
                  , style "align-items"      "center"
                  ]
                  [table [ style "text-align"     "center"
                         , style "vertical-align" "center"]
                       <| [tr [] [ td [style "text-align" "justify"] [text question]]]
                       ++ (L.map (\(msg, label) -> tr [] [td [ style "text-align" "right"
                                                             , style "width"      "350px"]
                                                             [ button [onClick msg] [text label]]])
                               buttons )]]]
  in
  case model.state of
    Help ->
        { title = "SAILORS"
        , body =  header ++ [div [] [drawBoard model]]
                  ++ (stackButtons styleBackground1 instructions1
                          [( GoBack, "Go Back")] )
        }
    InitialScreen ->
        { title = "SAILORS"
        , body =  header ++ [div [] [drawBoard model]]
                  ++ (stackButtons styleBackground2 ""
                          [ (ShowInstructions,    "Help")
                          , (NewGame,         "New Game")])
        }
    SelectLevel ->
        { title = "SAILORS"
        , body = header ++ [div [] [drawBoard model]]
                 ++ ( stackButtons styleBackground1 "Which level do you prefer?"
                          [ (SelectedLevel Beginner, "Beginner")
                          , (SelectedLevel Easy,     "Intermediate")
                          , (SelectedLevel Moderate, "Advanced")
                          , (SelectedLevel Strong,   "Expert")] )
        }
    SelectPlayer ->
        { title = "SAILORS"
        , body = header ++ [div [] [drawBoard model]]
                 ++ ( stackButtons styleBackground1 "Do you want to make the first move?"
                          [ (SelectedPlayer PlayerA, "I want to make the first move")
                          , (SelectedPlayer PlayerB, "Let the computer make the first move") ]
                    )
        }
    Started ->
        { title = "SAILORS"
        , body =  header ++ [div [] [drawBoard model]]
        }

-- | Common fn to update the model when a mouse click occurs or the computer plays a move.
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case winner model of
      WinnerPlayer player -> (model, Cmd.none)  -- Ends the game and the thinking from the computer.
      BothPlayers         -> (model, Cmd.none)  -- Ends the game and the thinking from the computer.
      NoWinner -> case msg of
         GoBack ->
             ( { model | state = model.pState, pState = model.state }, Cmd.none )
         NewGame ->
             ( { model | state = SelectLevel, pState = model.state }, Cmd.none )
         ShowInstructions    ->
             ( { model | state = Help, pState = model.state }, Cmd.none )
         SelectedLevel level ->
             ( { model | searchDepth = case level of
                                           Beginner ->  1
                                           Easy     ->  2
                                           Moderate ->  8
                                           Strong   -> 12
               , state = SelectPlayer, pState = model.state }, Cmd.none )
         SelectedPlayer player ->
             ( { model | computerPlayer = if player == PlayerA then PlayerB else PlayerA
               , playerTurn = PlayerA
               , state = Started, pState = model.state }
               , Cmd.none )

         MouseClick (x, y) -> if model.state == Started
                              then let newModel = analyzeClick (x, y) model
                                   in if newModel.msg /= ""
                                      then (newModel, Cmd.none)
                                      else ({ newModel | playerTurn = flipPlayer newModel.playerTurn }
                                           , Cmd.none)
                              else (model, Cmd.none)

         ComputerThink  ->  ({model | computerState = Thinking, msg = "I am thinking"}, Cmd.none)

         ComputerSearch  -> let newModel = searchAlphaBeta model -1000 1000 True model.searchDepth
                            in ({newModel | computerState = NotThinking
                                          , playerTurn = flipPlayer newModel.playerTurn
                                          , msg        = "" }
                                          , Cmd.none)

main : Program () Model Msg
main = Browser.document
       { init   = init
       , view   = view
       , update = update
       , subscriptions = subscriptions
       }


