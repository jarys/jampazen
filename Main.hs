{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import Graphics.Gloss (Display, Color, Picture(..),
    circleSolid, red, color, translate, dark,
    pictures, black, white, rotate)
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
import Data.List (elemIndex, mapAccumL, partition)
import Data.Tuple (swap)
import Data.Maybe (catMaybes)
import System.Random
import Control.Monad.State

import Control.Lens

import Physics
{-}
infixl 0 &
(&) :: a->(a->b)->b
(&) = flip ($)
-}
data Ball    = Ball Vector Vector Float
data Timer   = Timer
    { _total  :: Float
    , _actual :: Float 
    }
makeLenses ''Timer

data Body    = Body Ball Int

data Entity d = Entity {
      _pos    :: Vector
    , _vel    :: Vector
    , _rad    :: Float
    , _lifes  :: Int
    , _damage :: Int
    , _body :: d
    }
makeLenses ''Entity

type Bullet  = Entity ()
type Barrel  = Entity ()
type Player  = Entity ()
type Smoke   = Entity Timer

data Satelite = Satelite Float Float Float Float Float Float -- r w h rot v t

data MonsterData = MonsterData {
    _hull  :: [Satelite]
  , _shift :: Int
  }
makeLenses ''MonsterData

type Monster = Entity MonsterData




data Game = Game { 
      _player   :: Player
    , _keyboard :: Map.Map Key KeyState
    , _bullets  :: [Bullet]
    , _barrels  :: [Barrel]
    , _clicks   :: [Vector]
    , _smoke    :: [Smoke]
    , _seed     :: StdGen
    , _ticks    :: Int
    , _monsters :: [Monster]
    }
makeLenses ''Game

renderBody :: Entity e -> Picture
renderBody ent = translate x y $
                 color white $
                 circleSolid (ent^.rad)
                 where (x:+y) = ent^.pos

updateBody :: Float -> Entity e -> Entity e
updateBody time ent = ent & pos +~ (toVector time)*(ent^.vel)

collide :: Entity a -> Entity b -> Bool
collide a b = (realPart $ abs $ (a^.pos - b^.pos)) < (a^.rad + b^.rad)

isAlive :: Entity e -> Bool
isAlive ent = ent^.lifes > 0

window :: Display
window = InWindow "JampaZen" (500, 500) (5, 5) -- w, h, offset

background :: Color
background = black

initialState :: Game
initialState = generateMonster 0 $ Game
    { _player = Entity
        { _pos = 0
        , _vel = 0
        , _rad = 10
        , _lifes = 10
        , _damage = 1
        , _body = ()
        }
    , _keyboard = Map.fromList $ zip (map Char "wasd") (repeat Up)
    , _bullets  = []
    , _barrels  = []
    , _monsters = []
    , _clicks   = []
    , _smoke    = []
    , _seed     = mkStdGen 1
    , _ticks    = 0
    }

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState renderGame handleKeys updateGame

handleKeys :: Event -> Game -> Game
--handleKeys (EventKey (Char key) keystate _ _) game =
--    game { keyboard = Map.adjust (const keystate) (Char key) (keyboard game)}
handleKeys (EventKey (Char key) keystate _ _) =
    keyboard %~ Map.adjust (const keystate) (Char key)

handleKeys (EventKey (MouseButton LeftButton) Down _ (mx, my)) =
    clicks %~ (:) (mx:+my)

--handleKeys (EventKey (MouseButton LeftButton) Down _ (mx, my)) game =
--  game { _clicks = (mx:+my) : _clicks game}

handleKeys _ = id

{-} ##########
    #  GAME  #
    ########## -}

renderGame :: Game -> Picture
renderGame game =
    let (mx:+my) = game^.player.pos
        canmap x = map renderBody (game^.x)
    in  translate (-mx) (-my) $ 
        pictures $ concat $ [
          [renderBody (game^.player)]
        , map renderBody (game^.bullets)
        , map renderSmoke (game^.smoke)
        , map renderMonster (game^.monsters)
        ]

updateGame :: Float -> Game -> Game
updateGame time game =
    game & shot
         & removeSmoke
         & checkBarrelExplosions
         & onceUpon 4 bulletSmoke
         & bulletsKillMonsters
         & playerMove
         & player   %~ updateBody time
         & bullets  %~ map (updateBody time)
         & smoke    %~ map (updateSmoke time)
         & monsters %~ map (updateMonster time game)
         & ticks +~ 1


{-} ##########
    # RULLES #
    ########## -}

newBullet :: Vector -> Vector -> Bullet
newBullet p v = Entity
    { _pos=p, _vel=v, _rad=5, _lifes=1, _damage=1, _body=()}

shot :: Game -> Game
shot game = game & (clicks .~ [])
                 & (bullets %~ (++) newBullets)
      where dir :: Vector -> Vector
            dir c = 500*normalize (c - game^.player.pos)
            newBullets :: [Bullet]
            newBullets = (newBullet (game^.player.pos) . dir) <$> game^.clicks



removeSmoke :: Game -> Game
--removeSmoke game = game {
--  smoke = filter (\(Smoke _ timer)->not $ finished timer) $ smoke game } where

removeSmoke = smoke %~ filter ((>0) . (^.body.actual))

randomSt :: (Float, Float) -> State StdGen Float  
randomSt = state . randomR

createExplosionSmoke :: Float -> Vector -> State StdGen Smoke
createExplosionSmoke size pos = do
    pa <- randomSt (0, 2*pi)
    pr <- randomSt (0, 5*size)
    let p = mkPolar pr pa
    va <- randomSt (0, 2*pi)
    vr <- randomSt (10, 10*size)
    let v = mkPolar vr va
    r <- randomSt (10, 50)
    ttl <- randomSt (0.25, (sqrt size)/3)
    return Entity {_pos=p, _vel=v, _rad=r,
                   _lifes=1, _damage=0, _body=newTimer ttl}

createExplosion :: Float -> Vector -> Game -> Game
createExplosion size pos game =
    game & seed  .~ seed'
         & smoke %~ (++) newsmoke
    where
        (newsmoke, seed') = flip runState (game^.seed) $
            sequence $ take 30 $ repeat $ createExplosionSmoke size pos

-- two entities collide
(<..>) :: Entity a -> Entity b -> (Entity a, Entity b)
a <..> b | collide a b && isAlive a && isAlive b = (,)
              (a & lifes %~ (-) (b^.damage))
              (b & lifes %~ (-) (a^.damage))
         | otherwise = (a, b)

(<.+>) :: Entity a -> [Entity b] -> (Entity a, [Entity b]) 
(<.+>) = mapAccumL (<..>)

(<+.>) :: [Entity a] -> Entity b -> ([Entity a], Entity b) 
(<+.>) as b = swap $ b <.+> as

-- two list of entities collide
(<++>) :: [Entity a] -> [Entity b] -> ([Entity a], [Entity b])
(<++>) = mapAccumL (<+.>)


{-}
collisions game =
    game $ destructions
    where (bullets')
-}
bulletsKillMonsters game = game {-{
  bullets=bullets', monsters=monsters'} where
    (bullets', mosters') = bullets game !! 0 <+.> monsters game
-}
checkBarrelExplosions = id

{-
(<+>) :: Destroyable e : e -> e -> (e, e)
a <+> b | collides bullet monster
          && isAlive a && isAlive b = (,)
            (decrementLifes (getDamage b) a)
            (decrementLifes (getDamage a) b)
        | otherwise = (a, b)
-}

{-} #########
    # TIMER #
    ######### -}

newTimer :: Float -> Timer
newTimer time = Timer {_total=time, _actual=time}


onceUpon :: Int -> (Game -> Game) -> Game -> Game
onceUpon n f game | game^.ticks `mod` n == 0 = f game
                  | otherwise                = game  

bulletSmoke :: Game -> Game
bulletSmoke game = game & smoke %~ (++) (map bulletToSmoke $ game^.bullets)  
      where bulletToSmoke bullet = Entity{
            _pos=bullet^.pos
          , _vel=0
          , _rad=bullet^.rad
          , _lifes=1
          , _damage= 0
          , _body=newTimer 0.25
          }

{-} ##########
    # PLAYER #
    ########## -}

playerMove game = game & player.vel .~ 100*(keyboardToVector $ game^.keyboard)

stateToScalar :: KeyState -> Vector
stateToScalar Up = 1
stateToScalar Down = 0

keyToVector :: Key -> Vector
keyToVector (Char c) = maybe 0 (j^) (elemIndex c "asdw")

keyboardToVector :: Map.Map Key KeyState -> Vector
keyboardToVector = Map.foldrWithKey f (0:+0) where
    f k v a = a + (stateToScalar v)*(keyToVector k)

{-} ###########
    #  SMOKE  #
    ########### -}

updateSmoke :: Float -> Smoke -> Smoke
updateSmoke time = (updateBody time) . (body.actual -~ time)

renderSmoke smoke = translate x y $
               color white $
               circleSolid r
    where (x:+y) = smoke^.pos
          r = smoke^.rad * smoke^.body.actual / smoke^.body.total

{-} ###########
    # MONSTER #
    ########### -}

renderSatelite :: Satelite -> Picture
renderSatelite (Satelite r w h rot v t) =
    rotate rot $
    translate (w*cos t) (h*sin t)$
    color white $
    circleSolid r

updateSatelite :: Float -> Satelite -> Satelite
updateSatelite time (Satelite r w h rot v t) = (Satelite r w h rot v (t + v*time))

updateMonster :: Float -> Game -> Monster -> Monster
updateMonster time game monster = 
        monster & updateBody time
                & followPlayer 
                & body.hull %~ map (updateSatelite time)
        where
            pp = game^.player.pos
            mp = monster^.pos
            followPlayer = vel .~ 100*normalize (pp - mp)
              -- | (game^.ticks) + (monster^.body.shift) `mod` 60 /= 0 = id
              -- | abs (mp - pp) == 0 = id --deleni nulou
              -- | otherwise = vel .~ 100*normalize (pp - mp)

renderMonster :: Monster -> Picture
renderMonster monster =
    translate x y $ pictures $ renderSatelite <$> (monster^.body.hull)
    where (x:+y) = monster^.pos


generateSatelite :: State StdGen Satelite
generateSatelite = do
    r <- randomSt (10, 20)
    w <- randomSt (10, 50)
    h <- randomSt (10, 50)
    v <- randomSt (1, 5)
    t <- randomSt (0, 2*pi)
    rot <- randomSt (0, 2*pi)
    o <- signum <$> randomSt (-1, 1) -- orientation
    return $ Satelite r w h rot (o*v) t

generateMonster :: Vector -> Game -> Game
generateMonster p game =
    game & seed     .~ seed'
         & monsters %~ (:) Entity {
                _pos=p
              , _vel=0
              , _rad=50
              , _lifes=6
              , _damage=1
              , _body=MonsterData {
                    _hull=hull'
                  , _shift=game^.ticks
                  }
              }
    where (hull', seed') = flip runState (game^.seed) $
              sequence $ take 20 $ repeat $ generateSatelite