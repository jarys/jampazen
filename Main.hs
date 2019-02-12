{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

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

{-} ###########
    # PHYSICS #
    ########### -}

import Data.Complex

type Vector = Complex Float
j :: Vector
j = 0:+1

normalize :: RealFloat a => Complex a -> Complex a
normalize (0:+0) = 0:+0
normalize a = a / (sqrt $ a * conjugate a)

toxy :: Complex a -> (a, a)
toxy (x :+ y) = (x, y)

toVector :: Float -> Vector
toVector x = x:+0

{-} ##############
    # STRUCTURES #
    ############## -}

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

{-} ##########
    # ENTITY #
    ########## -}

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
isAlive ent = (ent^.lifes) > 0

{-} ##########
    #  GAME  #
    ########## -}

window :: Display
window = InWindow "JampaZen" (500, 500) (5, 5) -- w, h, offset

background :: Color
background = black

initialState :: Game
initialState = Game
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

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char key) keystate _ _) =
    keyboard %~ Map.adjust (const keystate) (Char key)
handleKeys (EventKey (MouseButton LeftButton) Down _ (mx, my)) =
    clicks %~ (:) (mx:+my)
handleKeys _ = id

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
    game & ifPlayerAlive shot
         & removeSmoke
         & onceUpon 4 bulletSmoke
         & ifPlayerAlive (onceUpon 50 spawnMonster)
         & playerMove
         & collisions
         & player   %~ updateBody time
         & bullets  %~ map (updateBody time)
         & smoke    %~ map (updateSmoke time)
         & monsters %~ map (updateMonster time game)
         & ticks    +~ 1

main :: IO ()
main = play window background fps initialState renderGame handleKeys updateGame


{-} ########
    # SHOT #
    ######## -}

newBullet :: Vector -> Vector -> Bullet
newBullet p v = Entity
    { _pos=p, _vel=v, _rad=5, _lifes=1, _damage=1, _body=()}

shot :: Game -> Game
shot game = game & (clicks .~ [])
                 & (bullets %~ (++) newBullets)
      where newBullets = (newBullet (game^.player.pos) . (500*) . normalize) <$> game^.clicks

{-} ##############
    # EXPLOSIONS #
    ############## -}

randomSt :: (Float, Float) -> State StdGen Float  
randomSt = state . randomR

generateExplosionSmoke :: Float -> Vector -> State StdGen Smoke
generateExplosionSmoke size pos = do
    pa <- randomSt (0, 2*pi)
    pr <- randomSt (0, 5*size)
    let p = mkPolar pr pa
    va <- randomSt (0, 2*pi)
    vr <- randomSt (10, 10*size)
    let v = mkPolar vr va
    r <- randomSt (10, 50)
    ttl <- randomSt (0.25, (sqrt size)/3)
    return Entity {_pos=p+pos, _vel=v, _rad=r,
                   _lifes=1, _damage=0, _body=newTimer ttl}

generateExplosion :: Float -> Vector -> Game -> Game
generateExplosion size pos game =
    game & seed  .~ seed'
         & smoke %~ (++) newsmoke
    where
        (newsmoke, seed') = flip runState (game^.seed) $
            sequence $ take 30 $ repeat $ generateExplosionSmoke size pos

{-} ##############
    # COLLISIONS #
    ############## -}

-- two entities collide
(<..>) :: Entity a -> Entity b -> (Entity a, Entity b)
a <..> b | collide a b && isAlive a && isAlive b = (,)
              (a & lifes %~ flip (-) (b^.damage))
              (b & lifes %~ flip (-) (a^.damage))
         | otherwise = (a, b)

(<.+>) :: Entity a -> [Entity b] -> (Entity a, [Entity b]) 
(<.+>) = mapAccumL (<..>)

(<+.>) :: [Entity a] -> Entity b -> ([Entity a], Entity b) 
(<+.>) as b = swap $ b <.+> as

-- two list of entities collide
(<++>) :: [Entity a] -> [Entity b] -> ([Entity a], [Entity b])
(<++>) = mapAccumL (<+.>)

smash :: Lens' Game [Entity a]
      -> Lens' Game [Entity b]
      -> (Entity a -> Game -> Game)
      -> (Entity b -> Game -> Game)
      -> Game -> Game

--smash as bs destroyA destroyB b game =
smash as bs destroyA destroyB game =
    game & as .~ aliveAs
         & bs .~ aliveBs
         & foldr (.) id destructions
    where
        (as', bs') = (game^.as) <++> (game^.bs)
        (aliveAs, deadAs) = partition isAlive as'
        (aliveBs, deadBs) = partition isAlive bs'
        destructions = map destroyA deadAs ++ map destroyB deadBs

collisions :: Game -> Game
collisions =
    smash bullets monsters destroyBullet destroyMonster

{-} #########
    # TIMER #
    ######### -}

newTimer :: Float -> Timer
newTimer time = Timer {_total=time, _actual=time}


onceUpon :: Int -> (Game -> Game) -> Game -> Game
onceUpon n f game | game^.ticks `mod` n == 0 = f game
                  | otherwise                = game  

{-} ##########
    # PLAYER #
    ########## -}

playerMove game = game & player.vel .~ 100*normalize (keyboardToVector $ game^.keyboard)

ifPlayerAlive :: (Game -> Game) -> Game -> Game
ifPlayerAlive f game = if isAlive (game^.player) then f game else game

stateToScalar :: KeyState -> Vector
stateToScalar Up = 1
stateToScalar Down = 0

keyToVector :: Key -> Vector
keyToVector (Char c) = maybe 0 (j^) (elemIndex c "asdw")

keyboardToVector :: Map.Map Key KeyState -> Vector
keyboardToVector = Map.foldrWithKey f (0:+0) where
    f k v a = a + (stateToScalar v)*(keyToVector k)

destroyPlayer :: Player -> Game -> Game
destroyPlayer player = generateExplosion 100 (player^.pos)

{-} ###########
    #  SMOKE  #
    ########### -}

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

updateSmoke :: Float -> Smoke -> Smoke
updateSmoke time = (updateBody time) . (body.actual -~ time)

renderSmoke smoke = translate x y $
               color white $
               circleSolid r
    where (x:+y) = smoke^.pos
          r = smoke^.rad * smoke^.body.actual / smoke^.body.total

removeSmoke :: Game -> Game
removeSmoke = smoke %~ filter ((>0) . (^.body.actual))

{-} ##########
    # BULLET #
    ########## -}

destroyBullet :: Bullet -> Game -> Game
destroyBullet = const id

{-} ###########
    # MONSTER #
    ########### -}

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
              , _lifes=3
              , _damage=1
              , _body=MonsterData {
                    _hull=hull'
                  , _shift=game^.ticks
                  }
              }
    where (hull', seed') = flip runState (game^.seed) $
              sequence $ take 20 $ repeat $ generateSatelite

spawnMonster :: Game -> Game
spawnMonster game = 
    game & generateMonster (mkPolar d a)
         & seed     .~ seed'
    where ((a, d), seed') = runState randomMonster (game^.seed)
          randomMonster = do
              a <- randomSt (0, 2*pi)
              d <- randomSt (1000, 2000)
              return (a, d)


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
            followPlayer
              | ((game^.ticks) + (monster^.body.shift)) `mod` 120 /= 0 = id
              | otherwise = vel .~ 100*normalize (pp - mp)

renderMonster :: Monster -> Picture
renderMonster monster =
    translate x y $ pictures $ renderSatelite <$> (monster^.body.hull)
    where (x:+y) = monster^.pos

destroyMonster :: Monster -> Game -> Game
destroyMonster monster = generateExplosion 15 (monster^.pos)