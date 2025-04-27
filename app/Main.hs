module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List

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

newtype Action = Action (Float -> Entity -> [Entity])

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
    isPaused :: Bool
}

despawn :: Action
despawn = Action $ \_ _ -> []

idle :: Action
idle = Action $ \_ entity -> [entity]

wait :: Float -> Action -> Action
wait time nextAction@(Action action) = Action $ \dt entity ->
    if time <= 0
        then action dt entity
        else [entity{action = wait (time - dt) nextAction}]

moveTo :: Point -> Float -> Action -> Action
moveTo target@(targetX, targetY) speed (Action action) = Action $ \dt entity@Entity{pos=(shipX, shipY)} ->
    let
        dx = targetX - shipX
        dy = targetY - shipY
        distance = sqrt (dx**2 + dy**2)
        velX = speed * dx / distance
        velY = speed * dy / distance
    in if distance < speed * dt
        then action dt entity { pos = target, vel = (0, 0) }
        else [entity { vel = (velX, velY) }]

shootTo :: Point -> Float -> Action -> Action
shootTo target speed nextAction = Action $ \_ entity@Entity{pos} ->
    [entity{action = nextAction}, bullet{pos = pos, action = moveTo target speed despawn}]

gruntPlan :: Action
gruntPlan = wait 1.0
    $ moveTo (200, 0) 100
    $ shootTo (0, -200) 200
    $ moveTo (-200, 0) 100
    $ wait 2.0
    $ shootTo (0, 200) 200
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

bullet :: Entity
bullet = Entity
    (0, 0)
    (0, 0)
    5
    1
    1
    5
    0
    idle
    $ color red $ circleSolid 5

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
        idle
        $ color blue $ rectangleSolid 50 50,
    pBullets = [],
    enemies = [
        grunt { pos = (0, 200) }
    ],
    eBullets = [],
    isPaused = False
}

render :: GameState -> Picture
render (GameState player@(Entity _ _ _ pMaxHP pHP _ score _ _) pBullets enemies eBullets isPaused) =
    if pHP <= 0 then
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
        pausedPic = if isPaused
            then translate (-90) (-370)
                $ scale 0.2 0.2
                $ color green
                $ text "Game Paused!"
            else blank
    in pictures [entityPics, pHealthPic, eHealthPics, scorePic, bulletCountPic, pausedPic]

tickScene :: Float -> GameState -> GameState
tickScene dt (GameState player pBullets enemies eBullets isPaused) = let
    tick :: Entity -> [Entity]
    tick entity@(Entity (x, y) (vx, vy) _ _ _ _ _ (Action action) _) =
        action dt entity{ pos = (x + vx * dt, y + vy * dt) }
    pEntities = tick player
    eEntities = filter (not . null) $ map tick enemies
    player' = head pEntities
    pBullets' = tail pEntities ++ concatMap tick pBullets
    enemies' = map head eEntities
    eBullets' = concatMap tail eEntities ++ concatMap tick eBullets
    in GameState player' pBullets' enemies' eBullets' isPaused

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
collideShips (GameState player pBullets enemies eBullets isPaused) = let
    (eBullets', newPlayer) = hitBy eBullets player
    (newEnemies, player') = hitBy enemies newPlayer
    (pBullets', enemies') = mapAccumL hitBy pBullets newEnemies
    in GameState player' pBullets' enemies' eBullets' isPaused

despawnEntities :: GameState -> GameState
despawnEntities (GameState player pBullets enemies eBullets isPaused) = let
    isAlive :: Entity -> Bool
    isAlive Entity{ health } = health > 0
    pBullets' = filter isAlive pBullets
    eBullets' = filter isAlive eBullets
    enemies' = filter isAlive enemies
    in GameState player pBullets' enemies' eBullets' isPaused

update :: Float -> GameState -> GameState
update seconds_lapsed game
    | isPaused game || health (player game) < 0 = game
    | otherwise = despawnEntities . collideShips . tickScene seconds_lapsed $ game

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'p') Down _ _) game = game { isPaused = not (isPaused game) }
handleKeys (EventKey (Char 'n') Down _ _) _ = initialState
handleKeys (EventKey (Char c) dir _ _) game = game { player = p { vel = velocity } }
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
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
