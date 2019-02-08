module Main(main) where

import Graphics.Gloss (Display, Color, Picture,
    circleSolid, red, color, translate, dark,
    pictures, black)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game (
      Event(EventKey)
    , Key(Char)
    , KeyState(Up, Down)
    , play
    , Display(InWindow)
    )

import qualified Data.Map.Strict as Map
import Data.Complex
type Vector = Complex Float

normalize a = a / (sqrt $ a * conjugate a)
window :: Display
window = InWindow "JampaZen" (500, 500) (5, 5) -- w, h, offset

background :: Color
background = black

-- | Data describing the state of the pong game. 
data PongGame = Game
    { player :: Vector
    , keyboard :: Map.Map Key KeyState
    }

toxy :: Complex a -> (a, a)
toxy (x :+ y) = (x, y)

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
    { player = 0 :+ 0
    , keyboard = Map.fromList $ zip (map Char "wasd") (repeat Down)
    }

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render Game {player=(x:+y)} =
    pictures [playerimage]
    where
        playerimage = translate x y $
                     color (dark red) $
                     circleSolid 10

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds = playerUpdate seconds --wallBounce . moveBall seconds

playerUpdate seconds game = game {player=pos'} where
    pos' = player game - 5*(keyboardToVector $ keyboard game)

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
stateToScalar :: KeyState -> Vector
stateToScalar Up = 1 :+ 0
stateToScalar Down = 0 :+ 0

keyToVector :: Key -> Vector
keyToVector (Char 'w') =  0 :+  1
keyToVector (Char 's') =  0 :+(-1)
keyToVector (Char 'a') =(-1):+  0
keyToVector (Char 'd') =  1 :+  0
keyToVector  _  =  0 :+  0



keyboardToVector :: Map.Map Key KeyState -> Vector
keyboardToVector = Map.foldrWithKey f (0:+0) where
    f k v a = a + (stateToScalar v)*(keyToVector k)

handleKeys :: Event -> PongGame -> PongGame
-- For an 's' keypress, reset the ball to the center.

handleKeys (EventKey key keystate _ _) game =
    game { keyboard = Map.adjust (const keystate) key (keyboard game)}

handleKeys _ game = game