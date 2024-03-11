--
-- Copyright 2023, Mario Latendresse, All rights reserved.
--
-- AI Sailors game.
--

module Board exposing (..)

import Core       exposing (..)
import Sailing    exposing (winner)

import List       as L  exposing (..)
import List.Extra as Le exposing (..)
import Tuple            exposing (first, second)
import String     as S
import Maybe            exposing (withDefault)
import Set

import Html       as H  exposing (div)
import Html.Attributes

import Svg             as Svg  exposing (svg, rect, polygon, text_, text)
import Svg.Attributes  as Svga exposing (..)

-- Translate a click at screen coordinate (x, y) on the board to a sail
-- kept in sailsPositionsA or B. If the sail does not correspond to the
-- current player, or is not on a sail, it is an error.
--
fromXYtoSailPos: (Float, Float) -> Model -> Result String SailPos
fromXYtoSailPos (x, y) model =
  -- The x and y coordinates should be over a currently displayed sail.
  let j               = floor  <| (x - toFloat bv.x0) / (toFloat bv.blockW)
      i               = floor  <| (y - toFloat bv.y0) / (toFloat bv.blockW)
      ((_, jA), dirA) = listGet i model.sailsPositionsA
      ((_, iB), dirB) = listGet j model.sailsPositionsB
   in if jA == j
      then if model.playerTurn == PlayerA
           then Ok ((i, jA), dirA)
           else Err "Click one of your sails, not one of mine"
      else if iB == i
           then if model.playerTurn == PlayerB
                then Ok ((j, iB), dirB)
                else Err "Click one of your sails, not one of mine"
           else Err "Please click one of your sails"

-- | Draw the board with its sails and messages according to the model.
drawBoard model =
  let iToS         = S.fromInt
      widthBoard   = bv.widthBoard
      heightBoard  = widthBoard
      xCenterBoard = widthBoard // 2 + bv.x0
      yBottomBoard = heightBoard + bv.y0
      wxy          = bv.squareW - 2*bv.inSpace
      aWinner      = case winner model of
                         WinnerPlayer _ -> True
                         BothPlayers    -> True
                         _              -> False
      index l i = withDefault (0, 0) (head (drop i l))
      dots n px py dx dy  = map (\i -> Svg.circle [ cx (iToS (px + i*dx))
                                                  , cy (iToS (py + i*dy)), r "3", fill "white" ] [])
                            (range 0 (n-1))
      pairDots n m px py dx dy wx wy = (dots n px py dx dy) ++ (dots m (px + wx) (py + wy) dx dy)
      pointsToString xys   = S.join " " (map (\(x, y) -> (iToS x)++","++(iToS y))
                                          (xys++[withDefault (0,0) (head xys)]))
      listDiff l1 l2 = Set.toList <| Set.diff (Set.fromList l1) (Set.fromList l2)
      arrivedSailsA = listDiff [1,2,3,4,5] <| map (\((k, _),_) -> k) model.sailsPositionsA
      arrivedSailsB = listDiff [1,2,3,4,5] <| map (\((k, _),_) -> k) model.sailsPositionsB
      sail x y direction strokeColor =
        case direction of
          ToLeft  ->
            [polygon [fill colorA, stroke strokeColor, strokeWidth "2",
                       points (pointsToString [ (x, y), (x + bv.sailL, y - bv.sailW //2)
                                              , (x + bv.sailL, y + bv.sailW //2)])] []]
          ToRight ->
            [polygon [fill colorA, stroke strokeColor, strokeWidth "2",
                       points (pointsToString [ (x, y), (x - bv.sailL, y - bv.sailW //2)
                                              , (x - bv.sailL, y + bv.sailW //2)])] []]
          ToUp ->
            [polygon [fill colorB, stroke strokeColor, strokeWidth "2",
                       points (pointsToString [ (x, y), (x + bv.sailW //2, y + bv.sailL)
                                              , (x - bv.sailW //2, y + bv.sailL)])] []]
          ToDown ->
            [polygon [fill colorB, stroke strokeColor, strokeWidth "2",
                      points (pointsToString [ (x, y), (x + bv.sailW //2, y - bv.sailL)
                                             , (x - bv.sailW //2, y - bv.sailL)])] []]
      centerBoard = concatMap
                    (\j ->
                         (map (\i ->
                                   rect [ x (iToS (bv.blockW * i + bv.x0 + bv.dockL + bv.inSpace))
                                        , y (iToS (bv.blockW * j + bv.y0 + bv.dockL + bv.inSpace))
                                        , Svga.width       (iToS bv.squareW)
                                        , Svga.height      (iToS bv.squareW)
                                        , fill        colorWater
                                        , stroke      colorWater
                                        , strokeWidth "4"
                                        ] [])
                              (range 0 3)))
                    (range 0 3)

      top = concatMap
            (\j -> let px = bv.blockW * j + bv.x0 + bv.dockL + bv.inSpace
                       py = bv.y0
                   in [rect [ x (iToS px)
                            , y (iToS py)
                            , Svga.width       (iToS bv.squareW)
                            , Svga.height      (iToS bv.dockL)
                            , fill        colorDock
                            , stroke      colorDock
                            , strokeWidth "4"
                            ] []]
                 ++ ((\(n, m) -> pairDots n m (px + bv.inSpace) (py + bv.inSpace) 0 10 wxy 0)
                         (index [(3, 1), (1,2), (2, 1), (1, 3)] j)))
            (range 0 3)

      bottom = concatMap
               (\j -> let px = bv.blockW * j + bv.x0 + bv.dockL + bv.inSpace
                          py = 4 * bv.blockW + bv.y0 + bv.dockL + bv.inSpace
                      in [rect [ x (iToS px)
                               , y (iToS py)
                               , Svga.width       (iToS bv.squareW)
                               , Svga.height      (iToS bv.dockL)
                               , fill        colorDock
                               , stroke      colorDock
                               , strokeWidth "4"
                               ] []]
                          ++  ((\(n, m) -> pairDots n m (px + bv.inSpace) (py + bv.dockL - bv.inSpace) 0 -10 wxy 0)
                             (index [(1, 3), (3, 2), (2, 3), (3, 1)] j)))
               (range 0 3)

      right = concatMap
              (\i -> let px = 4 * bv.blockW + bv.x0 + bv.dockL + bv.inSpace
                         py = i * bv.blockW + bv.y0 + bv.dockL + bv.inSpace
                     in [rect [ x (iToS px)
                              , y (iToS py)
                              , Svga.width       (iToS bv.dockL)
                              , Svga.height      (iToS bv.squareW)
                              , fill        colorDock
                              , stroke      colorDock
                              , strokeWidth "4"
                              ] []]
                         ++ ((\(n, m) -> pairDots n m (px + bv.dockL - bv.inSpace) (py + bv.inSpace) -10 0 0 wxy)
                             (index [(3, 1), (1, 2), (2, 1), (1, 3)] i)))
              (range 0 3)

      left = concatMap
             (\i -> let px = bv.x0
                        py = i * bv.blockW + bv.y0 + bv.dockL + bv.inSpace
                    in [rect [ x (iToS px)
                             , y (iToS py)
                             , Svga.width       (iToS bv.dockL)
                             , Svga.height      (iToS bv.squareW)
                             , fill        colorDock
                             , stroke      colorDock
                             , strokeWidth "4"
                             ] []]
                        ++ ((\(n, m) -> pairDots n m (px + bv.inSpace) (py + bv.inSpace) 10 0 0 wxy)
                                (index [(1, 3), (3, 2), (2, 3), (3, 1)] i)))
             (range 0 3)

      corners = concatMap
                (\(i, xi, yi) ->
                     [rect [ x           (iToS xi)
                           , y           (iToS yi)
                           , Svga.width  (iToS bv.dockL)
                           , Svga.height (iToS bv.dockL)
                           , fill        colorDock
                           , stroke      colorDock
                           , strokeWidth "4"
                           ] []]
                     ++ (case i of
                             0 -> (dots 3 (xi + bv.dockL - bv.inSpace) (yi + bv.inSpace) 0 10)
                                  ++ (dots 1 (xi + bv.inSpace) (yi + bv.dockL - bv.inSpace) 0 0)
                             1 -> (dots 3 (xi + bv.inSpace) (yi + bv.inSpace) 0 10)
                                  ++ (dots 3 (xi + bv.dockL - bv.inSpace) (yi + bv.dockL - bv.inSpace) -10 0)
                             2 -> (dots 3 (xi + bv.dockL - bv.inSpace) (yi + bv.inSpace) -10 0)
                                  ++ (dots 1 (xi + bv.inSpace) (yi + bv.dockL - bv.inSpace) 0 0)
                             _ -> (dots 1 (xi + bv.inSpace) (yi + bv.inSpace) 0 0)
                                  ++ (dots 1 (xi  + bv.dockL - bv.inSpace) (yi + bv.dockL - bv.inSpace) 0 0)))
                [ (0, bv.x0, bv.y0)
                , (1, bv.x0 + bv.dockL + 4 * bv.blockW + bv.inSpace, bv.y0)
                , (2, bv.x0 + bv.dockL + 4 * bv.blockW + bv.inSpace
                  , bv.y0 + bv.dockL + 4 * bv.blockW + bv.inSpace)
                , (3, bv.x0
                  , bv.y0 + bv.dockL + 4 * bv.blockW + bv.inSpace)]

      sails = concatMap
              (\((i, j), dir) ->
                   let strokeColor = if (i, j) == model.lastMove then "red" else "black"
                   in case dir of
                          ToLeft  -> sail (bv.x0 + j*bv.blockW - bv.inSpace)
                                     (bv.y0 + bv.dockL + (i-1)*bv.blockW + bv.inSpace //2)
                                     dir strokeColor
                          ToRight -> sail (bv.x0 + (j+1)*bv.blockW - bv.inSpace)
                                     (bv.y0 + bv.dockL + (i-1)*bv.blockW + bv.inSpace //2)
                                     dir strokeColor
                          ToUp   -> sail (bv.x0 + bv.dockL + (j-1)*bv.blockW + bv.inSpace //2)
                                    (bv.y0 + bv.dockL + (i-1)*bv.blockW - 2*bv.inSpace)
                                    dir strokeColor
                          ToDown -> sail (bv.x0 + bv.dockL + (j-1)*bv.blockW + bv.inSpace //2)
                                    (bv.y0 + (i+1)*bv.blockW - bv.inSpace)
                                    dir strokeColor)
              <| (map (\((j, i), dir) -> ((i, j), dir)) model.sailsPositionsB)
                  ++ model.sailsPositionsA
                  ++ (map (\j -> ((6, j), ToDown )) arrivedSailsB)
                  ++ (map (\i -> ((i, 6), ToRight)) arrivedSailsA)
      textBottom = [text_ [x <| iToS xCenterBoard, y <| iToS <| yBottomBoard + 40, textAnchor "middle"]
                        (if model.computerPlayer == PlayerB
                         then [ text "My Sails"]
                         else [ text "Your Sails" ])]
      textRight  = [text_ [x <| iToS <| widthBoard + bv.x0 + 30, y <| iToS <| heightBoard // 2 + bv.y0 + 10
                          , textAnchor "left"]
                        (if model.computerPlayer == PlayerA
                         then [ text "My Sails"]
                         else [ text "Your Sails"])]
      msgTextTop = case winner model of
                     WinnerPlayer player ->
                         if model.computerPlayer == player
                         then "I Win!"
                         else "You Win!"
                     BothPlayers -> "The Game is a Draw!"
                     NoWinner -> if model.msg == ""
                                 then if model.playerTurn == model.computerPlayer
                                      then ""
                                      else "Please click a sail to play"
                                 else model.msg
      textTop    = [text_ [x <| iToS xCenterBoard, y <| iToS <| bv.y0 - 20, textAnchor "middle"
                          , fill "red"]
                        [text msgTextTop]]
      textOnTop  = [text_ [x <| iToS xCenterBoard, y <| iToS <| bv.y0 - 40, textAnchor "middle"
                          , fill "red"]
                    [text <| if aWinner
                             then ""
                             else if model.estValue >= 120
                                  then "I will Win!"
                                  else if model.estValue <= -120
                                       then "You can Win!"
                                       else if model.estValue >= 3
                                            then "I am ahead"
                                            else if model.estValue <= -3
                                                 then "You are ahead"
                                                 else ""]]
      board      = centerBoard ++ top ++ bottom ++ left ++ right ++ corners
      texts      = textBottom ++ textRight ++ textTop ++ textOnTop
  in div []
      [
        svg [ viewBox "0 0 600 700"
            , Svga.width  "600"
            , Svga.height "700"
            ]
         <| board ++ sails ++ texts
      ]
