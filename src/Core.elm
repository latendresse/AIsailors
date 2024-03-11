--
-- Copyright 2023, Mario Latendresse, All rights reserved.
--
-- AI Sailors game.
--

module Core exposing (..)

import List       as L  exposing (..)
import List.Extra as Le exposing (..)
import Maybe            exposing (..)

type Msg = MouseClick (Float, Float) | ComputerThink | ComputerSearch | SelectedLevel GameLevel
         | SelectedPlayer Player | ShowInstructions | NewGame | GoBack

-- Player A is the starting player. It is the player with the sails going horizontally on the board.
-- Player B sails are going vertically on the board.
type Player     = PlayerA | PlayerB
type Direction  = ToUp    | ToDown  | ToLeft | ToRight

type Winner     = BothPlayers | WinnerPlayer Player | NoWinner
type GameState  = InitialScreen | SelectLevel | SelectPlayer | Started | Help
type GameLevel  = Beginner | Easy | Moderate | Strong


flipPlayer : Player -> Player
flipPlayer player =
  if player == PlayerA then PlayerB else PlayerA

-- | Flip a model to change player. The estimated value and value does not change because
-- it is always relative to the computer.
flipTurnModel model =
  {model | playerTurn = flipPlayer model.playerTurn}

type ComputerState = NotThinking | Thinking

type alias Loc         = (Int, Int)          -- ^ Location of a sail, first component is row or column, the key.
                                             -- ^ For playerA, the first component is the row, for player B
                                             -- ^ it is the column of the board.
type alias SailPos     = (Loc, Direction)    -- ^ Sail position for a specific player.
type alias MoveFrom    = Loc                 -- ^ A location of a sail to move, for current player.
type alias MoveTo      = Loc                 -- ^ A location to move a sail to, for current player.
type alias MoveToBacks = (MoveTo, List Int)  -- ^ MoveTo and a list of sails to move back, for current player.

-- The column or row of a sail. For B, a row, for A a column.
type alias SailsPos = List SailPos

type alias Model =
  { playerTurn        : Player
  , sumSailDistancesA : Int              -- ^ The sum of the distances sailed by the sails for player A.
  , sumSailDistancesB : Int              -- ^ The sum of the distances sailed by the sails for player B.
  , sailsPositionsA   : SailsPos         -- ^
  , sailsPositionsB   : SailsPos
  , sumBackMovesA     : Int
  , sumBackMovesB     : Int
  , value             : Int              -- ^ Value of the model relative to playerTurn.
  , estValue          : Int              -- ^ Estimated value from the Alpha/Beta Search.
  , msg               : String
  , computerPlayer    : Player           -- ^ Who the computer is really playing.
  , computerState     : ComputerState
  , lastMove          : (Int, Int)       -- ^ Location (row, column) on the board of the last move.
  , state             : GameState
  , pState            : GameState        -- ^ Keep the last state. Implement the GoBack msg.
  , searchDepth       : Int              -- ^ The alpha/beta searching depth level to use.
  }

invalidSailPos = ((-1, -1), ToLeft)

-- The listGet and listSet operate as an association list. The first element of the tuple is the key
-- (either a row or column number on the board) and the second element is the index location of a
-- sail on the row or column.
listGet: Int -> SailsPos -> SailPos
listGet k sailsPos =
    foldl (\((k2, v2), dir) d -> if k == k2 then ((k2, v2), dir) else d)
        invalidSailPos sailsPos

-- | Replace in sailsPos, the sailPos. The key is the first value of the Loc in sailPos. If the
-- index location is 6, and the direction dir is ToRight or ToDown, it means that the sail has
-- arrived back and can be removed from the board.
listSet: SailPos -> SailsPos -> SailsPos
listSet ((k, v), dir) sailsPos =
    foldl (\((k2, v2), dir2) sailsPos2 ->
               if k2 == k
               then if v == 6 && (dir == ToDown || dir == ToRight)
                    then sailsPos2 -- Sail arrived to its final dock.
                    else ((k,   v), dir) ::sailsPos2
               else ((k2, v2), dir2)::sailsPos2)
        [] sailsPos

invalidModel =
  { playerTurn         = PlayerA
  , sumSailDistancesA  = 0
  , sumSailDistancesB  = 0
  , sailsPositionsA    = []
  , sailsPositionsB    = []
  , sumBackMovesA      = 0
  , sumBackMovesB      = 0
  , value              = 0
  , estValue           = 0
  , msg                = ""
  , computerPlayer     = PlayerA
  , computerState      = NotThinking
  , lastMove           = (-1, -1)
  , state              = Started
  , pState             = Started
  , searchDepth        = 0
  }

shouldNotBeUsed = ((0, (0,0)), invalidModel)

headModels l = withDefault shouldNotBeUsed <| L.head l
tailModels l = withDefault [] <| L.tail l

-- | Should a sail change its direction.
whichDirection dir pos player =
  case player of
    PlayerA -> if pos == 0 then ToRight else dir
    PlayerB -> if pos == 0 then ToDown  else dir

type alias BoardLook = { squareW    : Int
                       , inSpace    : Int
                       , dockL      : Int
                       , blockW     : Int
                       , x0         : Int
                       , y0         : Int
                       , sailW      : Int
                       , sailL      : Int
                       , widthBoard : Int
                       }

-- The base parameters for the look of the board.
squareW = 40
inSpace = 10
bv : BoardLook
bv = { squareW    = squareW
     , dockL      = (3*squareW) // 2
     , inSpace    = inSpace
     , blockW     = squareW + inSpace
     , sailW      = (3*inSpace) // 2
     , sailL      = squareW
     , x0         = 40
     , y0         = 50
     , widthBoard = 4*(squareW + inSpace) + 3*squareW
     }

colorA     = "lime"
colorB     = "fuchsia"
colorWater = "aqua"
colorDock  = "maroon"
