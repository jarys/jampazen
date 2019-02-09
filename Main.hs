module Main(main) where

import Graphics.Gloss (Display, Color, Picture,
    circleSolid, red, color, translate, dark,
    pictures, black, white)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game (
      Event(EventKey)
    , MouseButton(..)
    , Key(Char, MouseButton)
    , KeyState(Up, Down)
    , play
    , Display(InWindow)
    )

import qualified Data.Map.Strict as Map
import Data.List (elemIndex)

import Physics

data Ball = Ball Vector Vector Float
type Bullet = Ball

data Player = Player {body::Ball}

data Game = Game { 
           player:: Player
         , balls :: [Ball]
         , keyboard :: Map.Map Key KeyState
         , bullets :: [Bullet]
         , clicks :: [Vector]
         }

class Entity e where
  render :: e -> Picture  
  update :: Float -> Game -> e -> e

window :: Display
window = InWindow "JampaZen" (500, 500) (5, 5) -- w, h, offset

background :: Color
background = black

initialState :: Game
initialState = Game
    { player = Player { body=Ball 0 0 10
                      }
    , keyboard = Map.fromList $ zip (map Char "wasd") (repeat Up)
    , balls = [Ball (100:+100) 0 10, Ball ((-100):+100) 0 100]
    , bullets = []
    , clicks = []
    }

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys initUpdate

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char key) keystate _ _) game =
    game { keyboard = Map.adjust (const keystate) (Char key) (keyboard game)}

handleKeys (EventKey (MouseButton LeftButton) Down _ (mx, my)) game =
    game { clicks = (mx:+my) : clicks game}
handleKeys _ game = game

{-} ##########
    #  GAME  #
    ########## -}

instance Entity Game where
  render game =
    let Ball (mx:+my) _ _ = body $ player game
        camera :: Entity e => e -> Picture
        camera = translate (-mx) (-my) . render
        cammap x = map camera $ x game
    in  pictures $ concat [
          [camera $ player game],
          cammap balls,
          cammap bullets
        ]


  update time _ game =
    let up   :: Entity e => e -> e
        up    = update time game
        ups x = map up $ x game
    in  shot time $
      game { player = up $ player game
             , balls = ups balls
             , bullets = ups bullets
             }

initUpdate time game = update time game game

{-} ##########
    # ROLLES #
    ########## -}

shot :: Float -> Game -> Game
shot time game = game {
        clicks=[],
        bullets=[Ball (playerPos game) (100*(normalize c)) 5 | c<-clicks game]
                ++ bullets game
      }


{-} ##########
    #  BALL  #
    ########## -}

instance Entity Ball where
  update time _ (Ball p v r) = Ball (p + (toVector time)*v) v r
  render (Ball (x:+y) v r) = translate x y $
                             color white $
                             circleSolid r 

{-} ##########
    # PLAYER #
    ########## -}

instance Entity Player where
  update time game player =
      Player { body=(Ball p' v' r)} where
          Ball p v r = body player
          p' = p + (toVector time)*v
          v' = 100*(keyboardToVector $ keyboard game)

  render Player{body=b} = color red $ render b

stateToScalar :: KeyState -> Vector
stateToScalar Up = 1
stateToScalar Down = 0

keyToVector :: Key -> Vector
keyToVector (Char c) = maybe 0 (j^) (elemIndex c "asdw")

keyboardToVector :: Map.Map Key KeyState -> Vector
keyboardToVector = Map.foldrWithKey f (0:+0) where
    f k v a = a + (stateToScalar v)*(keyToVector k)

playerPos :: Game -> Vector
playerPos game = p where Ball p _ _ = body $ player game 