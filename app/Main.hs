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

data Entity = Entity {
    pos :: Point,
    vel :: Vector,
    radius :: Float,
    maxHealth :: Int,
    health :: Int,
    damage :: Int,
    score :: Int,
    mesh :: Picture
} deriving Show

data GameState = GameState {
    player :: Entity,
    pBullets :: [Entity],
    enemies :: [Entity],
    eBullets :: [Entity],
    isPaused :: Bool
} deriving Show

grunt :: Entity
grunt = Entity {
    pos = (0, 0),
    vel = (0, 0),
    radius = 25,
    maxHealth = 50,
    health = 50,
    damage = 1,
    score = 10,
    mesh = color red $ rectangleSolid 50 50
}

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
        $ color blue $ rectangleSolid 50 50,
    pBullets = [],
    enemies = [
        grunt { pos = (0, 200) }
    ],
    eBullets = [],
    isPaused = False
}

render :: GameState -> Picture
render (GameState player@(Entity _ _ _ pMaxHP pHP _ score _) pBullets enemies eBullets isPaused) =
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
    else pictures [pHealthPic, entityPics, eHealthPics, scorePic, bulletCountPic, pausedPic]
    where
        pHealthPic =
            translate 0 ((-windowHeightFloat)/2)
            $ color green
            $ rectangleSolid (windowWidthFloat * fromIntegral pHP / fromIntegral pMaxHP) (windowHeightFloat/20)
        entityPics = pictures [
            translate x y mesh
            | Entity (x, y) _ _ _ _ _ _ mesh
            <- [player] ++ pBullets ++ enemies ++ eBullets]
        eHealthPics = pictures [translate x (y + 40)
            $ color green
            $ rectangleSolid (fromIntegral eHP) 5
            | Entity (x, y) _ _ _ eHP _ _ _ <- enemies]
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
            then translate (-90) (-380)
                $ scale 0.2 0.2
                $ color green
                $ text "Game Paused!"
            else blank

tickScene :: Float -> GameState -> GameState
tickScene delta (GameState player pBullets enemies eBullets isPaused) =
    GameState player' pBullets' enemies' eBullets' isPaused
    where
        tick :: Entity -> Entity
        tick entity@(Entity (x, y) (vx, vy) _ _ _ _ _ _) =
            entity { pos = (x + vx * delta, y + vy * delta) }
        player' = tick player
        pBullets' = map tick pBullets
        enemies' = map tick enemies
        eBullets' = map tick eBullets

isIntersect :: Entity -> Entity -> Bool
isIntersect (Entity (shipX, shipY) _ shipRad _ _ _ _ _) (Entity (enemyX, enemyY) _ enemyRad _ _ _ _ _) =
    sqrt ((shipX-enemyX)**2 + (shipY-enemyY)**2) < shipRad + enemyRad

hitBy :: [Entity] -> Entity -> ([Entity], Entity)
hitBy enemies ship = (enemies', ship') where
    (hits, misses) = partition (isIntersect ship) enemies
    totalDmg = sum $ map damage hits
    ship' = ship { health = health ship - totalDmg }
    hitEnemies = map (\e -> e { health = health e - damage ship }) hits
    enemies' = hitEnemies ++ misses

collideShips :: GameState -> GameState
collideShips (GameState player pBullets enemies eBullets isPaused) =
    GameState player' pBullets' enemies' eBullets' isPaused
    where
        (eBullets', newPlayer) = hitBy eBullets player
        (newEnemies, player') = hitBy enemies newPlayer
        (pBullets', enemies') = mapAccumL hitBy pBullets newEnemies

despawnEntities :: GameState -> GameState
despawnEntities (GameState player pBullets enemies eBullets isPaused) =
    GameState player pBullets' enemies' eBullets' isPaused
    where
        isInside :: Entity -> Bool
        isInside (Entity (x, y) _ _ _ _ _ _ _) =
            x > -(windowWidthFloat/2) &&
            x < (windowWidthFloat/2) &&
            y > -(windowHeightFloat/2) &&
            y < (windowHeightFloat/2)
        pBullets' = filter isInside pBullets
        eBullets' = filter isInside eBullets
        enemies' = filter (\(Entity _ _ _ _ eHP _ _ _) -> eHP > 0) enemies

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
