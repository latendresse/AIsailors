--
-- Copyright 2023, Mario Latendresse, All rights reserved.
--
-- AI Sailors game.
--

module Sailing exposing (moveSailFrom, sailsPositions, winner, nextPosition
                        , possibleMovesOver, playSailTo)

import Core exposing (..)

import List       as L  exposing (..)
import List.Extra as Le exposing (..)
import Tuple      as T  exposing (..)
import String     as S

-- | Do a move of a sail from a given position. A player or a computer move.
moveSailFrom: Model -> SailPos -> Model
moveSailFrom model sailPos =
    let moveTo = nextPosition model sailPos
    in playSailTo model moveTo

-- | Giving a sail at location (i, j) returns the pair (nextI, nextJ) of the next position of that
-- sail, if it were played. Consider the opponent sails. Only one of the index i or j is changed.
nextPosition: Model -> SailPos -> MoveToBacks
nextPosition model ((ij, ijT), dir) =
  let (_, sailsPosT) = sailsPositions model
      (ij2, _, inc)  = limitIndex model ((ij, ijT), dir)
  in dirRange ij (ijT + inc) ij2 inc sailsPosT []

-- | Return a list of moves that can go over the opponent sail(s).
possibleMovesOver: Model -> List MoveToBacks
possibleMovesOver model =
  let sailsPos    = first <| sailsPositions model
      movesOverTo = filter (\(_, back) -> back /= []) <|
                    map (nextPosition model)
                        sailsPos
  in movesOverTo

-- | Play the given sail to the given location and apply the sail back moves for the current player.
-- The direction of the sail may change.
playSailTo: Model -> MoveToBacks -> Model
playSailTo model ((ij, ijT), backMoves) =
    let (sailsPos, sailsPosT) = sailsPositions model
        (_, dir)              = listGet ij sailsPos
        newDir                = whichDirection dir ijT model.playerTurn
        newLocs               = listSet ((ij, ijT), newDir) sailsPos
        newLocsT              = multiSailsBack backMoves sailsPosT
        sumBackMoves          = sum <| map (\bm -> nbBackMoves model bm) backMoves
    in if model.playerTurn == PlayerA
       then { model | sailsPositionsA = newLocs
            , sailsPositionsB = newLocsT
            , sumBackMovesA = model.sumBackMovesA + sumBackMoves
            , lastMove = (ij,  ijT) }
       else { model | sailsPositionsB = newLocs
            , sailsPositionsA = newLocsT
            , sumBackMovesB = model.sumBackMovesB + sumBackMoves
            , lastMove = (ijT, ij) }

-- Return the number of moves back for a back move at row i or column j of the opponent sails.
nbBackMoves: Model -> Int -> Int
nbBackMoves m ij =
    case m.playerTurn of
        PlayerA -> let ((_, ijT), dir) = listGet ij m.sailsPositionsB
                   in if dir == ToUp
                      then case ij of
                              1 -> 6 - ijT
                              2 -> if 3 <= ijT then 1
                                   else 2
                              3 -> if 4 <= ijT then 1
                                   else if 2 <= ijT then 2
                                        else 3
                              4 -> if 3 <= ijT then 1
                                   else 2
                              5 -> 6 - ijT
                              _ -> 0
                      else case ij of
                              1 -> if 3 <= ijT then 2
                                   else 1
                              2 -> ijT
                              3 -> if 4 <= ijT then 3
                                   else if 2 <= ijT then 2
                                        else 1
                              4 -> ijT
                              5 -> if 3 <= ijT then 2
                                   else 1
                              _ -> 0
        PlayerB -> let ((_, ijT), dir) = listGet ij m.sailsPositionsA
                   in if dir == ToLeft
                      then case ij of
                               1 -> if 3 <= ijT then 1
                                    else 2
                               2 -> 6 - ijT
                               3 -> if 4 <= ijT then 1
                                    else if 2 <= ijT then 2
                                         else 3
                               4 -> 6 - ijT
                               5 -> if 3 <= ijT then 1
                                    else 2
                               _ -> 0
                      else case ij of
                              1 -> ijT
                              2 -> if 3 <= ijT then 2
                                   else 1
                              3 -> if 4 <= ijT then 3
                                   else if 2 <= ijT then 2
                                        else 1
                              4 -> ijT
                              5 -> if 3 <= ijT then 2
                                        else 1
                              _ -> 0

-- | The index ij is either a row (i) or column (j). The index ijT is the index on the row or
-- column, that is, (ij, ijT) is the location of the sail to move. The index ij2 is the end index
-- for ijT. The inc is either -1 or 1, for ijT. The list locsT are the locations of the opponent
-- sails, on a row or column. The list moveBackLocs are sails to move back, if any.
--
dirRange: Int -> Int -> Int -> Int -> SailsPos -> List Int
        -> (MoveTo, List Int)
dirRange ij ijT ij2 inc locsT moveBackLocs =
    let ((_, ijTT), dir) = listGet ijT locsT
        ijTnext          = ijT + inc
    in if ijTT == ij   -- Collision?
       then dirRange ij ijTnext ijTnext inc locsT (ijT::moveBackLocs)
       else if (inc == 1 && ijT >= ij2 || inc == -1 && ijT <= ij2)  -- At or passed the end index?
            then ((ij, ijT), moveBackLocs)  -- Cannot go further.
            else dirRange ij ijTnext ij2 inc locsT moveBackLocs

-- | Bring back to the last dock several sails.
multiSailsBack: List Int -> SailsPos -> SailsPos
multiSailsBack rowCols sailsPos =
    foldl (\ij sailsPosF -> sailBack ij sailsPosF) sailsPos rowCols

-- | Sails back to the last docking spot on column or row ij.
sailBack: Int -> SailsPos -> SailsPos
sailBack ij sailsPos =
    let (_, dir) = listGet ij sailsPos
    in listSet ((ij, if member dir [ToRight, ToDown] then 0 else 6), dir) sailsPos

-- | Disregarding the opponent's sails, return the maximum or minimum index from the sailPos,
-- the direction that a sail can navigate to, and the index increment.
limitIndex : Model -> SailPos -> (Int, Direction, Int)
limitIndex model ((k, ij), dir)  =
  let nb  = nbS ((k, ij), dir)
      inc = if nb < 0 then -1 else if nb > 0 then 1 else 0
      ij2 = Basics.min 6 <| Basics.max 0 (ij + nb)
  in (ij2, dir, inc)

-- | Return the sails positions for (PlayerA, PlayerB) or vice versa.
sailsPositions model =
  if model.playerTurn == PlayerA
  then (model.sailsPositionsA, model.sailsPositionsB)
  else (model.sailsPositionsB, model.sailsPositionsA)

-- | How many steps can a sail go?
nbS : SailPos -> Int
nbS ((k, ij), dir)  =
  case dir of
    ToLeft -> case k of
                1 ->  -3
                2 ->  -1
                3 ->  -2
                4 ->  -1
                5 ->  -3
                _ -> 0
    ToRight -> if ij == 6 then 0
               else case k of
                        1 ->  1
                        2 ->  3
                        3 ->  2
                        4 ->  3
                        5 ->  1
                        _ -> 0
    ToUp    -> case k of
                 1 ->  -1
                 2 ->  -3
                 3 ->  -2
                 4 ->  -3
                 5 ->  -1
                 _ -> 0
    ToDown -> if ij == 6 then 0
              else case k of
                       1 ->  3
                       2 ->  1
                       3 ->  2
                       4 ->  1
                       5 ->  3
                       _ -> 0

-- | Returns the winner, if any.
winner: Model -> Winner
winner model =
    let wA = (length model.sailsPositionsA) == 1 -- If four sails have arrived.
        wB = (length model.sailsPositionsB) == 1
    in if wA && wB
       then BothPlayers
       else if wA then WinnerPlayer PlayerA
            else if wB then WinnerPlayer PlayerB
                 else NoWinner

