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
import Data.Maybe (catMaybes)
import System.Random
import Control.Monad.State  

import Physics

data Ball    = Ball Vector Vector Float
data Body    = Body { bodyBall   :: Ball
                    , bodyLifes  :: Int
                    , bodyDamage :: Int
                    }
type Bullet  = Body
type Barrel  = Body
data Smoke   = Smoke Ball Timer


data Satelite = Satelite Float Float Float Float Float Float-- r w h rot v t
data Monster = Monster { monsterBody :: Body
                       , hull        :: [Satelite]
                       , shift       :: Int
                       }

data Game = Game { 
           player   :: Body
         , keyboard :: Map.Map Key KeyState
         , bullets  :: [Bullet]
         , barrels  :: [Barrel]
         , clicks   :: [Vector]
         , smoke    :: [Smoke]
         , seed     :: StdGen
         , ticks    :: Int
         , monsters :: [Monster]
         }

class Entity e where
    render   :: e -> Picture  
    update   :: Float -> Game -> e -> e
    destroy  :: e -> e -> Game -> Game
    body     :: e -> Body
    setBody  :: Body -> e -> e

    collider      :: e -> Ball
    lifes, damage :: e -> Int
    position      :: e -> Vector
    collider = bodyBall   . body
    lifes    = bodyLifes  . body
    damage   = bodyDamage . body
    position ent = p where Ball p _ _ = collider ent

    setBall :: Ball -> e -> e
    setBall ball ent = setBody (body ent){bodyBall=ball} ent

    decreaseLifes :: Int -> e -> e
    decreaseLifes n ent = setBody (Body b (l - n) d) ent
        where Body b l d = body ent

    collides :: e -> e -> Bool
    collides a b = (realPart $ abs $ (p1 - p2)) < (r1 + r2)
        where Ball p1 _ r1 = collider a
              Ball p2 _ r2 = collider b

    isAlive :: e -> Bool
    isAlive ent = lifes ent > 0

    updateBall :: Float -> Game -> e -> e
    updateBall time game ent =
        setBall (Ball (p + (toVector time)*v) v r) ent
        where Ball p v r = collider ent

    renderBall :: e -> Picture
    renderBall ent = translate x y $
                     color white $
                     circleSolid r
                     where (Ball (x:+y) _ r) = collider ent

window :: Display
window = InWindow "JampaZen" (500, 500) (5, 5) -- w, h, offset

background :: Color
background = black

initialState :: Game
initialState = generateMonster 0 $ Game
    { player = Body { bodyBall  = Ball 0 0 10
                    , bodyLifes = 10
                    , bodyDamage = 0
                    }
    , keyboard = Map.fromList $ zip (map Char "wasd") (repeat Up)
    , bullets  = []
    , barrels  = [Body (Ball (x:+y) 0 10) 1 0 | x <- [100, 150 .. 1000], y <- [100, 150 .. 1000]]
    , monsters = []
    , clicks   = []
    , smoke    = []
    , seed     = mkStdGen 1
    , ticks    = 0
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
    let (mx:+my) = position $ player game
        camera :: Entity e => e -> Picture
        camera = translate (-mx) (-my) . render
        cammap x = map camera $ x game
    in  pictures $ concat [
          [camera $ player game]
        , cammap bullets
        , cammap barrels
        , cammap smoke
        , cammap monsters
        ]


  update time _ game =
    let up   :: Entity e => e -> e
        up    = update time game
        ups x = map up $ x game
    in  shot time $
        removeSmoke $
        checkBarrelExplosions $
        onceUpon 4 bulletSmoke $
        bulletsKillMonsters $
        playerMove $
        game { player = up $ player game
             , bullets = ups bullets
             , barrels = ups barrels
             , smoke = ups smoke
             , monsters = ups monsters
             , ticks = ticks game + 1
             }

initUpdate time game = update time game game

{-} ##########
    # RULLES #
    ########## -}

shot :: Float -> Game -> Game
shot _ game = game {
        clicks=[],
        bullets=[Body (Ball (position $ player game) (500*(normalize c)) 5) 1 1 | c<-clicks game]
                ++ bullets game
      }

removeSmoke :: Game -> Game
removeSmoke game = game {
  smoke = filter (\(Smoke _ timer)->not $ finished timer) $ smoke game } where

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
    return $ Smoke (Ball (p + pos) v r) $ newTimer ttl

createExplosion :: Float -> Vector -> Game -> Game
createExplosion size pos game =
    game {seed=seed', smoke=smoke game ++ newsmoke} where
        (newsmoke, seed') = flip runState (seed game) $
            sequence $ take 30 $ repeat $ createExplosionSmoke size pos

{-
checkBarrelExplosions :: Game -> Game
checkBarrelExplosions game = foldl (.) id explosions $
    game {bullets=bullets', barrels=barrels'} where
        onfly :: Bullet -> [Barrel] -> (Either Bullet Vector, [Barrel])
        onfly bullet   []   = (Left bullet, [])
        onfly bullet (x:xs) | ballsCollide bullet x = (Right $ ballPos x, xs)
                            | otherwise = (\(a, b)->(a, x:b)) $ onfly bullet xs
        (bulletOrVector, barrels') = flip runState (barrels game) $
            sequence $ map (state . onfly) $ bullets game
        bullets' = [b | Left b <- bulletOrVector]
        explosions = [createExplosion 10 v | Right v <- bulletOrVector]

bulletsKillMonsters :: Game -> Game
bulletsKillMonsters game = foldl (.) id explosions $
    game {bullets=bullets', monsters=aliveMonsters++monsters'} where
        onfly :: Bullet -> [Monster] -> (Either Bullet Monster, [Monster])
        onfly bullet   []   = (Left bullet, [])
        onfly bullet (x:xs) | ballsCollide bullet (monsterBody x) = (Right x, xs)
                            | otherwise = (\(a, b)->(a, x:b)) $ onfly bullet xs
        (bulletOrMonster, monsters') = flip runState (monsters game) $
            sequence $ map (state . onfly) $ bullets game
        bullets' = [b | Left b <- bulletOrMonster]
        (aliveMonsters, deadMonsters) = partition monsterAlive $
            [damageMonster 1 m | Right m <- bulletOrMonster]
        explosions = map (createExplosion 20 . monsterPos) deadMonsters
-}

-- two entities collide

(<+>) :: Entity a => a -> a -> (a, a)
a <+> b | collides a b && isAlive a && isAlive b = (,)
              (decreaseLifes (damage a) b)
              (decreaseLifes (damage b) a)
        | otherwise = (a, b)
-- entity and list of entities collide
(<+.>) :: Entity a => [a] -> a -> (a, [a])
as <+.> b = mapAccumL (<+>) b as
-- two list of entities collide
--(<++>) :: Entity a => [a] -> [a] -> ([a], [a])
--as <++> bs          = mapAccumL (<+.>) as bs
    --where as <+.> b = mapAccumL (<+>)   b as

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

    
data Timer = Timer Float Float
newTimer time = Timer time time
decrease time (Timer total actual) = Timer total (actual - time)
finished (Timer total actual) = actual < 0
restart (Timer _ total) = newTimer total

{-
instance Entity Timer where
    update time _ (Timer total actual) = Timer total (actual - time)
    render _ = Blank
-}

onceUpon :: Int -> (Game -> Game) -> Game -> Game
onceUpon n f game | ticks game `mod` n == 0 = f game
                  | otherwise               = game  

bulletSmoke :: Game -> Game
bulletSmoke game = game {
    smoke = (map (bulletToSmoke.collider) $ bullets game) ++ (smoke game) }
      where bulletToSmoke (Ball p _ r) = Smoke (Ball p 0 r) (newTimer 0.25)

{-} ##########
    # PLAYER #
    ########## -}

playerMove game = game{
  player= setBall (Ball p v' r) $ player game} 
      where Ball p v r = collider $ player game
            v' = 100*(keyboardToVector $ keyboard game)

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

smokeBall :: Smoke -> Ball
smokeBall (Smoke (Ball p v r) (Timer total ttl)) =
    Ball p v (r*ttl/total)

instance Entity Smoke where
  update time game (Smoke ball timer) =
    updateBall time game $ Smoke ball (decrease time timer)
  render (Smoke (Ball (x:+y) v r) (Timer total ttl)) = 
            translate x y $
            color white $
            circleSolid (r*ttl/total)

  body (Smoke ball _) = Body ball 0 0 
  setBody (Body ball _ _) (Smoke _ timer) = Smoke ball timer



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

instance Entity Monster where
    update time game monster = 
        updateBall time game $
        followPlayer $
        monster {
          hull = map (updateSatelite time) $ hull monster
        } where
            pp = position $ player game
            Ball mp _ r = collider monster
            followPlayer
              | (ticks game + shift monster) `mod` 60 /= 0 = id
              | abs (mp - pp) == 0 = id --deleni nulou
              | otherwise = setBall (Ball mp (100*normalize (pp - mp)) r)

    render monster =
        translate x y $ pictures $ renderSatelite <$> hull monster
        where (x:+y) = position monster

    setBody body' monster = monster{monsterBody=body'}
    body = monsterBody
    destroy _ _ = id


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
generateMonster pos game = game {
    monsters = Monster {
        monsterBody = Body (Ball pos 0 50) 6 1,
        hull = hull',
        shift = ticks game
    } : monsters game, seed =seed' } where
    (hull', seed') = flip runState (seed game) $
        sequence $ take 20 $ repeat $ generateSatelite

{-} ##########
    #  BODY  #
    ########## -}

instance Entity Body where
    update = updateBall
    render = renderBall
    destroy _ _ = id
    body = id
    setBody body' _ = body'
