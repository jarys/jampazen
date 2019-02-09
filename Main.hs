module Main(main) where

import Graphics.Gloss (Display, Color, Picture(..),
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
import Data.List (elemIndex, mapAccumL)
import System.Random
import Control.Monad.State  

import Physics

data Ball   = Ball Vector Vector Float
type Bullet = Ball
type Barrel  = Ball
data Smoke = Smoke Ball Float Float


data Player = Player {body::Ball}

data Game = Game { 
           player:: Player
         , balls :: [Ball]
         , keyboard :: Map.Map Key KeyState
         , bullets :: [Bullet]
         , barrels :: [Barrel]
         , clicks :: [Vector]
         , smoke :: [Smoke]
         , seed :: StdGen
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
    , balls    = [Ball ((-100):+100) 0 100]
    , bullets  = []
    , barrels  = [Ball (100:+100) 0 10]
    , clicks   = []
    , smoke    = []
    , seed     = mkStdGen 1
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
          [camera $ player game]
        , cammap balls
        , cammap bullets
        , cammap barrels
        , cammap smoke
        ]


  update time _ game =
    let up   :: Entity e => e -> e
        up    = update time game
        ups x = map up $ x game
    in  shot time $
        removeSmoke $
        checkBarrelExplosions $
        game { player = up $ player game
             , balls = ups balls
             , bullets = ups bullets
             , barrels = ups barrels
             , smoke = ups smoke
             }

initUpdate time game = update time game game

{-} ##########
    # RULLES #
    ########## -}

shot :: Float -> Game -> Game
shot _ game = game {
        clicks=[],
        bullets=[Ball (playerPos game) (100*(normalize c)) 5 | c<-clicks game]
                ++ bullets game
      }

removeSmoke :: Game -> Game
removeSmoke game = game {
  smoke = filter ((>0) . ttl) $ smoke game } where
    ttl :: Smoke -> Float
    ttl (Smoke _ t _) = t

randomSt :: (Float, Float) -> State StdGen Float  
randomSt = state . randomR

createExplosionSmoke :: Vector -> State StdGen Smoke
createExplosionSmoke m = do
    pa <- randomSt (0, 2*pi)
    pr <- randomSt (0, 10)
    let p = mkPolar pr pa
    va <- randomSt (0, 2*pi)
    vr <- randomSt (10, 500)
    let v = mkPolar vr va
    r <- randomSt (10, 50)
    ttl <- randomSt (0.25, 1)
    return $ Smoke (Ball (p + m) v r) ttl ttl

createExplosion :: Int -> Vector -> Game -> Game
createExplosion size pos game =
    game {seed=seed', smoke=smoke game ++ newsmoke} where
        (newsmoke, seed') = runState generatePaticles $ seed game
        generatePaticles = sequence $ take 20 $ repeat $ createExplosionSmoke pos

checkBarrelExplosions :: Game -> Game
checkBarrelExplosions game =
    explosions $ game {bullets=bullets', barrels=barrels'} where
        onfly :: Bullet -> [Barrel] -> (Either Bullet Vector, [Barrel])
        onfly bullet [] = (Left bullet, [])
        onfly bullet (x:xs) | ballsCollide bullet x = (Right $ ballPos x, xs)
                            | otherwise = (\(a, b)->(a, x:b)) $ onfly bullet xs
        (bulletOrVector, barrels') = flip runState (barrels game) $
            sequence (map (state . onfly) (bullets game))
        bullets' = [b | Left b <- bulletOrVector]
        explosions = foldl (.) id [createExplosion 10 v | Right v <- bulletOrVector]


{-} ##########
    #  BALL  #
    ########## -}

instance Entity Ball where
  update time _ (Ball p v r) = Ball (p + (toVector time)*v) v r
  render (Ball (x:+y) v r) = translate x y $
                             color white $
                             circleSolid r

ballsCollide (Ball p1 _ r1) (Ball p2 _ r2) =
    (realPart $ abs $ (p1 - p2)) < (r1 + r2)

ballPos (Ball p _ _) = p

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

{-} ###########
    #  SMOKE  #
    ########### -}

instance Entity Smoke where
  update time game (Smoke ball ttl total) =
    Smoke (update time game ball) (ttl - time) total
  render (Smoke (Ball p v r) ttl total)
    | ttl > 0   = render $ Ball p v (r*ttl/total)
    | otherwise = Blank

{-} ###########
    #  OTHER  #
    ########### -}