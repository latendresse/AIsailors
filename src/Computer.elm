--
-- Copyright 2023, Mario Latendresse, All rights reserved.
--
-- AI Sailors game.
--
module Computer exposing (searchAlphaBeta)

import Core     exposing (..)
import Sailing  exposing (..)

import List               exposing (..)
import Tuple              exposing (..)
import Maybe              exposing (withDefault)
import Maybe.Extra as Mbe exposing (isJust, isNothing, values)
import String as S

-- | Search the best move from the given model. The value returned is the estimation of the future
-- value of the model, but the model returned is the new model to accomplish it with its current
-- value.  The estimation is not necessarily the same as the value of the returned model.
--
-- The maximization levels correspond to the calls with parameter mazimization sets to True.
-- These are the levels to maximize their estimated value.
--
-- Alpha (a), is the best value found so far at a previous level for the maximization levels.
-- Beta (b), is the best value found so far at a previous level for the minimization levels.
--
searchAlphaBeta : Model -> Int -> Int -> Bool -> Int -> Model
searchAlphaBeta model a b maximize depth =
  case winner model of
    WinnerPlayer player ->
        if player == model.computerPlayer
        then { model | estValue =  120 }
        else { model | estValue = -120 }
    BothPlayers -> { model | estValue = 0 }
    NoWinner ->
        if depth == 0
        then -- The basic value becomes the estimated value at the leaves of the search.
            let quiescentModel = quiesce model
            in { quiescentModel | estValue = quiescentModel.value }
        else
            let modelsOrd   = playPossibleMoves model      -- It cannot be empty.
                headModel l = withDefault model <| head l
                tailModel l = withDefault []    <| tail l
            in if maximize
               then -- Maximize branch.
                   let loopMax models alpha maxModel =
                         if isEmpty models
                         then maxModel
                         else let tryModel    = headModel models
                                  oppModel    = flipTurnModel tryModel
                                  tryModelEst = searchAlphaBeta oppModel alpha b False (depth - 1)
                                  newEst      = tryModelEst.estValue
                                  newMaxModel =
                                    if newEst > maxModel.estValue
                                    then { tryModel | estValue = newEst }
                                    else maxModel
                                  newAlpha    = Basics.max alpha newEst
                              in if newAlpha >= b  -- Abandon searching at that level.
                                 then newMaxModel  -- Beta cutoff.
                                 else loopMax (tailModel models) newAlpha newMaxModel
                   in loopMax modelsOrd a { model | estValue = -1000 }
               else -- Opponent branch.
                   let loopMin models beta minModel =
                           if isEmpty models
                           then minModel
                           else let tryModel    = headModel models
                                    oppModel    = flipTurnModel tryModel
                                    tryModelEst = searchAlphaBeta oppModel a beta True (depth - 1)
                                    newOpEst    = tryModelEst.estValue
                                    newMinModel = if newOpEst < minModel.estValue
                                                  then tryModelEst
                                                  else minModel
                                    newBeta     = Basics.min beta newMinModel.estValue
                                in if newBeta <= a
                                   then newMinModel -- Alpha cutoff.
                                   else loopMin (tailModel models) newBeta newMinModel
                   in loopMin modelsOrd b { model | estValue = 1000 }

-- | Recursively move sails that go over opponent sails until no such move can be done. That
-- quiescent state should be done at the leafs of an alpha-beta search to get a better
-- estimation. Return the model that creates the maximum estimation.
--
quiesce: Model -> Model
quiesce model =
 let moves  = possibleMovesOver model -- Could be empty.
     models = map (\m -> quiesce <| evaluateModel <| playSailTo model m)
              moves  -- If moves is empty, there is no recursive call to quiesce.
 in withDefault model <| head <| reverse <| sortBy .value models


-- | Play the possible moves from the model, returns an order list of evaluated models.
-- The list could be empty.
playPossibleMoves: Model -> List Model
playPossibleMoves model =
    let sailsPos = first <| sailsPositions model   -- All possible moves. Could be empty.
    in playMoves model sailsPos

-- | Play the given moves from a location and return the resulting models in decreasing order of
-- their value.
playMoves: Model -> SailsPos  -> List Model
playMoves model sailsPos =
    let modelsEval = sortBy .value <| map (playMoveEval model) sailsPos
        modelsOrd  = if model.playerTurn == model.computerPlayer
                     then reverse <|  modelsEval -- Best valued model first.
                     else modelsEval
    in modelsOrd

playMoveEval model sailPos =
   moveSailFrom model sailPos |> evaluateModel

--| An evaluation of the model based on the number of steps the sails went back.
--
evaluateModel: Model -> Model
evaluateModel model =
    case winner model of
        WinnerPlayer player ->
            if player == model.computerPlayer
            then {model | value =  120 }
            else {model | value = -120 }
        BothPlayers -> { model | value = 0 }
        NoWinner -> case model.computerPlayer of
                        PlayerA -> { model | value = model.sumBackMovesA - model.sumBackMovesB }
                        PlayerB -> { model | value = model.sumBackMovesB - model.sumBackMovesA }

