module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 300
height = 300
offset = 100

paddleWidth = 26
paddleHeight = 86
ballRadius = 10

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the pong game. 
data PongGame = Game
    { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
    , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
    , player1 :: Float           -- ^ Left player paddle height.
                                                             -- Zero is the middle of the screen. 
    , player2 :: Float           -- ^ Right player paddle height.
    } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
    { ballLoc = (-10, 30)
    , ballVel = (1, -3)
    , player1 = 40
    , player2 = -80
    }

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
    pictures [ball, walls,
                        mkPaddle rose 120 $ player1 game,
                        mkPaddle orange (-120) $ player2 game]
    where
        --  The pong ball.
        ball = uncurry translate (ballLoc game) $
                     color ballColor $
                     circleSolid ballRadius
        ballColor = dark red

        --  The bottom and top walls.
        wall :: Float -> Picture
        wall offset =
            translate 0 offset $
                color wallColor $
                    rectangleSolid 270 10

        wallColor = greyN 0.5
        walls = pictures [wall 150, wall (-150)]

        --  Make a paddle of a given border and vertical offset.
        mkPaddle :: Color -> Float -> Float -> Picture
        mkPaddle col x y = pictures
            [ translate x y $ color col $ rectangleSolid 26 86
            , translate x y $ color paddleColor $ rectangleSolid 20 80
            ]

        paddleColor = light (light blue)

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
                 -> PongGame -- ^ The initial game state
                 -> PongGame -- ^ A new game state with an updated ball position
-- | Update the ball position using its current
moveBall seconds game = game { ballLoc = (x', y') }
    where
        -- Old locations and velocities.
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        -- New locations.
        x' = x + vx * seconds
        y' = y + vy * seconds

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds = wallBounce . moveBall seconds

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.

type Radius = Float 
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision    = y - radius <= -fromIntegral width / 2 
        bottomCollision = y + radius >=  fromIntegral width / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
    where

        -- The old velocities.
        (vx, vy) = ballVel game

        vy' = if wallCollision (ballLoc game) ballRadius
                    then -vy
                    else  vy

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') _ _ _) game =
  game { ballLoc = (0, 0) }
-- Do nothing for all other events.
handleKeys (EventKey (Char a) _ _ _) game = game
handleKeys _ game = game
{-}
-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleCollision :: Game -> Bool
paddleCollison = bx - ballRadius < p
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
        where
                radius = 10
                (vx, vy) = ballVel game
--}