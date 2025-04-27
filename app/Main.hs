module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List
import System.Random
import Data.Maybe

windowWidth, windowHeight, fps :: Int
windowWidth = 600
windowHeight = 800
fps = 60

windowWidthFloat, windowHeightFloat, playerSpeed :: Float
windowWidthFloat = fromIntegral windowWidth
windowHeightFloat = fromIntegral windowHeight
playerSpeed = 200

offset :: (Int, Int)
offset = (400, 200)

window :: Display
window = InWindow "Shape Shooter" (windowWidth, windowHeight) offset

background :: Color
background = black

newtype Action = Action (Float -> Entity -> IO [Entity])

data Entity = Entity {
    pos :: Point,
    vel :: Vector,
    radius :: Float,
    maxHealth :: Int,
    health :: Int,
    damage :: Int,
    score :: Int,
    action :: Action,
    mesh :: Picture
}

data GameState = GameState {
    player :: Entity,
    pBullets :: [Entity],
    enemies :: [Entity],
    eBullets :: [Entity],
    currentWeapon :: String,
    isPaused :: Bool
}

despawn :: Action
despawn = Action $ \_ _ -> pure []

idle :: Action
idle = Action $ \_ entity -> pure [entity]

wait :: Float -> Action -> Action
wait time nextAction@(Action action) = Action $ \dt entity ->
    if time <= 0
        then action dt entity
        else pure [entity{ action = wait (time - dt) nextAction }]

move :: Action -> Action
move _ = Action $ \dt entity@Entity{ pos = (shipX, shipY), vel = (velX, velY) } ->
    pure [entity { pos = (shipX + velX * dt, shipY + velY * dt) }]

moveTo :: Point -> Float -> Action -> Action
moveTo target@(targetX, targetY) speed (Action action) = Action $ \dt entity@Entity{ pos = (shipX, shipY) } ->
    let
        dx = targetX - shipX
        dy = targetY - shipY
        distance = sqrt (dx**2 + dy**2)
        velX = speed * dx / distance
        velY = speed * dy / distance
    in if distance < speed * dt
        then action dt entity { pos = target, vel = (0, 0) }
        else pure [entity { vel = (velX, velY) }]

moveUntil :: Float -> Action -> Action
moveUntil time nextAction@(Action action) = Action $ \dt entity@Entity{ pos = (shipX, shipY), vel = (velX, velY) } ->
    if time <= 0
        then action dt entity
        else pure [entity { pos = (shipX + velX * dt, shipY + velY * dt), action = moveUntil (time - dt) nextAction }]

shoot :: Entity -> Action -> Action
shoot bullet nextAction = Action $ \_ entity@Entity{ pos } ->
    pure [entity{ action = nextAction }, bullet{ pos = pos, action = action bullet }]

shootTo :: Point -> Entity -> Float -> Action -> Action
shootTo target bullet speed nextAction = Action $ \_ entity@Entity{ pos } ->
    pure [entity{ action = nextAction }, bullet{ pos = pos, action = moveTo target speed despawn }]

shootRandom :: Entity -> Float -> Action -> Action
shootRandom bullet speed nextAction = Action $ \_ entity@Entity{ pos } -> do
    angle <- randomRIO (0, 2 * pi)
    pure [entity{ action = nextAction }, bullet{ pos = pos, vel = (speed * cos angle, speed * sin angle) }]

repeatedlyShoot :: Entity -> Float -> Float -> Action -> Action
repeatedlyShoot bullet cooldown interval _ = Action $ \dt entity@Entity{ pos } ->
    pure $ if cooldown <= 0
        then [entity{ action = repeatedlyShoot bullet interval interval idle }, bullet{ pos = pos, action = action bullet }]
        else [entity{ action = repeatedlyShoot bullet (cooldown - dt) interval idle }]

repeatedlyShootRandom :: Entity -> Float -> Float -> Action -> Action
repeatedlyShootRandom bullet cooldown interval _ = Action $ \dt entity@Entity{ pos } -> do
    angle <- randomRIO (0, 2 * pi)
    let speed = 300
        newBullet = bullet { pos = pos, vel = (speed * cos angle, speed * sin angle), action = moveUntil 0.4 despawn }
    pure $ if cooldown <= 0
        then [entity{ action = repeatedlyShootRandom bullet interval interval idle }, newBullet]
        else [entity{ action = repeatedlyShootRandom bullet (cooldown - dt) interval idle }]

standardPlayerPlan :: Action
standardPlayerPlan = repeatedlyShoot (playerColor rubber{ vel = (0, 300) }) 0 0.5 idle

bombPlan :: Entity -> Float -> Action
bombPlan fragment splits
    | splits > 1 = moveUntil travelTime
        $ shootRandom frag fragSpeed
        $ shootRandom frag fragSpeed
        $ shootRandom frag fragSpeed
        $ shootRandom frag fragSpeed
        despawn
    | otherwise = moveUntil travelTime despawn
    where
        travelTime = splits / 10
        frag = fragment {
            action = bombPlan fragment (splits - 1),
            mesh = scale (splits/5) (splits/5) $ mesh fragment,
            damage = round splits
        }
        fragSpeed = 30 * splits

roombaPlayerPlan :: Action
roombaPlayerPlan = repeatedlyShootRandom (playerColor rubber) 0.5 0 idle

playerBomb :: Entity
playerBomb = playerColor bomb{ vel = (0, 150), action = moveUntil 0.5 $ bombPlan playerBomb 4, mesh = mesh bomb }

bombPlayerPlan :: Action
bombPlayerPlan = repeatedlyShoot playerBomb 1.0 1.0 idle

gruntPlan :: Action
gruntPlan = wait 1.0
    $ moveTo (200, 0) 100
    $ shootTo (0, -200) (enemyColor rubber) 200
    $ moveTo (-200, 0) 100
    $ wait 2.0
    $ shootTo (0, 200) (enemyColor rubber) 200
    $ wait 1.0
    despawn

grunt :: Entity
grunt = Entity
    (0, 0)
    (0, 0)
    25
    50
    50
    1
    10
    gruntPlan
    $ color red $ rectangleSolid 50 50

rubber :: Entity
rubber = Entity
    (0, 0)
    (0, 0)
    5
    1
    1
    5
    0
    (move despawn)
    $ circleSolid 5

bomb :: Entity
bomb = Entity
    (0, 0)
    (0, 0)
    10
    1
    1
    20
    0
    (moveUntil 2 despawn)
    $ circleSolid 10

enemyColor :: Entity -> Entity
enemyColor entity = entity { mesh = color (light $ light red) $ mesh entity }

playerColor :: Entity -> Entity
playerColor entity = entity { mesh = color (light $ light blue) $ mesh entity }

initialState :: GameState
initialState = GameState {
    player = Entity
        (0, 0)
        (0, 0)
        25
        100
        100
        2
        0
        standardPlayerPlan
        $ color blue $ rectangleSolid 50 50,
    pBullets = [],
    enemies = [
        grunt { pos = (0, 200) }
    ],
    eBullets = [],
    currentWeapon = "Rubber Gun",
    isPaused = False
}

render :: GameState -> IO Picture
render (GameState player@(Entity _ _ _ pMaxHP pHP _ score _ _) pBullets enemies eBullets weapon isPaused) =
    pure $ if pHP <= 0 then
        pictures [
            translate (-200) 50
                $ scale 0.5 0.5
                $ color white
                $ text "Game Over!",
            translate (-200) (-50)
                $ scale 0.3 0.3
                $ color yellow
                $ text ("Score: " ++ show score)
        ]
    else let
        entityPics = pictures [
            translate x y mesh
            | Entity (x, y) _ _ _ _ _ _ _ mesh
            <- [player] ++ pBullets ++ enemies ++ eBullets]
        pHealthPic =
            translate 0 ((-windowHeightFloat)/2)
            $ color green
            $ rectangleSolid (windowWidthFloat * fromIntegral pHP / fromIntegral pMaxHP) (windowHeightFloat/20)
        eHealthPics = pictures [translate x (y + 40)
            $ color green
            $ rectangleSolid (fromIntegral eHP) 5
            | Entity (x, y) _ _ _ eHP _ _ _ _ <- enemies]
        scorePic =
            translate ((-windowWidthFloat)/2+5) (windowHeightFloat/2-30)
            $ scale 0.2 0.2
            $ color white
            $ text ("Score: " ++ show score)
        bulletCountPic = translate ((-windowWidthFloat)/2+5) (windowHeightFloat/2-60)
            $ scale 0.1 0.1
            $ color yellow
            $ text ("Bullets in the scene: " ++ show (length eBullets + length pBullets))
        weaponPic = translate ((-windowWidthFloat)/2+5) ((-windowHeightFloat)/2+30)
            $ scale 0.15 0.15
            $ color cyan
            $ text ("Current Weapon: " ++ weapon)
        pausedPic = if isPaused
            then translate (-90) (-370)
                $ scale 0.2 0.2
                $ color green
                $ text "Game Paused!"
            else blank
    in pictures [entityPics, pHealthPic, eHealthPics, scorePic, bulletCountPic, pausedPic, weaponPic]

tick :: Float -> Entity -> IO [Entity]
tick dt entity@Entity{ pos = (x, y), vel = (vx, vy), action = Action action } =
    action dt entity{ pos = (x + vx*dt, y + vy*dt) }

tickScene :: Float -> GameState -> IO GameState
tickScene dt (GameState player pBullets enemies eBullets weapon isPaused) = do
    pEntities <- tick dt player
    let (player', addedPBullets) = fromMaybe (error "Player entity should not be deleted!") $ uncons pEntities
    pLists <- mapM (tick dt) pBullets
    let pBullets' = addedPBullets ++ concat pLists
    eLists <- mapM (tick dt) enemies
    let (enemies', addedEBullets) = unzip $ mapMaybe uncons eLists
    eBulletLists <- mapM (tick dt) eBullets
    let eBullets' = concat addedEBullets ++ concat eBulletLists
    pure $ GameState player' pBullets' enemies' eBullets' weapon isPaused

isIntersect :: Entity -> Entity -> Bool
isIntersect (Entity (shipX, shipY) _ shipRad _ _ _ _ _ _) (Entity (enemyX, enemyY) _ enemyRad _ _ _ _ _ _) =
    sqrt ((shipX-enemyX)**2 + (shipY-enemyY)**2) < shipRad + enemyRad

hitBy :: [Entity] -> Entity -> ([Entity], Entity)
hitBy enemies ship = (enemies', ship') where
    (hits, misses) = partition (isIntersect ship) enemies
    totalDmg = sum $ map damage hits
    ship' = ship { health = health ship - totalDmg }
    hitEnemies = map (\e -> e { health = health e - damage ship }) hits
    enemies' = hitEnemies ++ misses

collideShips :: GameState -> GameState
collideShips (GameState player pBullets enemies eBullets weapon isPaused) = let
    (eBullets', newPlayer) = hitBy eBullets player
    (newEnemies, player') = hitBy enemies newPlayer
    (pBullets', enemies') = mapAccumL hitBy pBullets newEnemies
    in GameState player' pBullets' enemies' eBullets' weapon isPaused

despawnEntities :: GameState -> GameState
despawnEntities (GameState player pBullets enemies eBullets weapon isPaused) = let
    isInside :: Entity -> Bool
    isInside Entity{ health, pos = (x, y) } =
        health > 0 &&
        x > -(windowWidthFloat/2) &&
        x < (windowWidthFloat/2) &&
        y > -(windowHeightFloat/2) &&
        y < (windowHeightFloat/2)
    player' = player { score = score player + sum (map score deadEnemies) }
    pBullets' = filter isInside pBullets
    eBullets' = filter isInside eBullets
    (enemies', deadEnemies) = partition ((> 0) . health) enemies
    in GameState player' pBullets' enemies' eBullets' weapon isPaused

update :: Float -> GameState -> IO GameState
update seconds_lapsed game
    | isPaused game || health (player game) < 0 = pure game
    | otherwise = do
        game' <- tickScene seconds_lapsed game
        pure $ despawnEntities . collideShips $ game'

handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (Char 'p') Down _ _) game = pure game { isPaused = not (isPaused game) }
handleKeys (EventKey (Char 'n') Down _ _) _ = pure initialState
handleKeys (EventKey (Char c) dir _ _) game = pure game { player = p { vel = velocity, action = plan }, currentWeapon = weapon }
    where
        p = player game
        (vx, vy) = vel p
        sign :: Float
        sign = if dir == Down then 1 else -1
        velocity = case c of
            'w' -> (vx, vy + sign * playerSpeed)
            's' -> (vx, vy - sign * playerSpeed)
            'a' -> (vx - sign * playerSpeed, vy)
            'd' -> (vx + sign * playerSpeed, vy)
            _   -> (vx, vy)
        (plan, weapon) = case c of
            'h' -> (standardPlayerPlan, "Rubber Gun")
            'j' -> (bombPlayerPlan, "Bomb Launcher")
            'k' -> (standardPlayerPlan, "Flame Thrower")
            'l' -> (roombaPlayerPlan, "Roomba")
            _ -> (action p, currentWeapon game)
handleKeys _ game = pure game

main :: IO ()
main = playIO window background fps initialState render handleKeys update
