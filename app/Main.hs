module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed
import Data.List

windowWidth, windowHeight, fps :: Int
windowWidth = 600
windowHeight = 800
fps = 60

windowWidthFloat, windowHeightFloat :: Float
windowWidthFloat = fromIntegral windowWidth
windowHeightFloat = fromIntegral windowHeight

offset :: (Int, Int)
offset = (400, 200)

window :: Display
window = InWindow "Shape Shooter" (windowWidth, windowHeight) offset

background :: Color
background = black

data Bullet = Bullet {
    bulletPos :: (Float, Float),
    bulletVel :: (Float, Float),
    bulletRadius :: Float,
    bulletDamage :: Int,
    bulletIsFriendly :: Bool,
    bulletMesh :: Picture
} deriving (Show)

data Gun = Gun {
    gunName :: String,
    bullet :: Bullet,
    fireRate :: Float,
    cooldown :: Float
} deriving (Show)

data Ship = Ship {
    shipPos :: (Float, Float),
    shipVel :: (Float, Float),
    shipHP :: Int,
    shipGun :: Gun,
    shipRadius :: Float,
    shipScore :: Int,
    shipMesh :: Picture
} deriving Show

data Player = Player {
    playerShip :: Ship,
    playerGuns :: [Gun],
    playerMaxSpeed :: Float
} deriving Show

data GameState = GameState {
    player :: Player,
    bullets :: [Bullet],
    enemies :: [Ship],
    score :: Int,
    isPaused :: Bool
} deriving Show

grunt :: Ship
grunt = Ship {
    shipPos = (0, 0),
    shipVel = (0, 0),
    shipHP = 50,
    shipGun = Gun {
        gunName = "Grunt Gun",
        bullet = Bullet {
            bulletPos = (0, 0),
            bulletVel = (0, -300),
            bulletRadius = 5,
            bulletDamage = 5,
            bulletIsFriendly = False,
            bulletMesh = color (light red) $ circleSolid 5
        },
        fireRate = 1,
        cooldown = 0
    },
    shipRadius = 25,
    shipScore = 50,
    shipMesh = color red $ rectangleSolid 50 50
}

initialState :: GameState
initialState = GameState {
    player = Player {
        playerShip = Ship {
            shipPos = (0, 0),
            shipVel = (0, 0),
            shipHP = 100,
            shipGun = Gun {
                gunName = "Basic Gun",
                bullet = Bullet {
                    bulletPos = (0, 0),
                    bulletVel = (0, 400),
                    bulletRadius = 5,
                    bulletDamage = 10,
                    bulletIsFriendly = True,
                    bulletMesh = color (light blue) $ circleSolid 5
                },
                fireRate = 0.5,
                cooldown = 0
            },
            shipRadius = 25,
            shipScore = 0,
            shipMesh = color blue $ rectangleSolid 50 50
        },
        playerGuns = [],
        playerMaxSpeed = 200
    },
    bullets = [],
    enemies = [
        grunt{ shipPos = (-250, 425), shipVel = (0, -50) },
        grunt{ shipPos = (-150, 425), shipVel = (0, -60) },
        grunt{ shipPos = (-50, 425), shipVel = (0, -70) },
        grunt{ shipPos = (50, 425), shipVel = (0, -80) },
        grunt{ shipPos = (150, 425), shipVel = (0, -90) },
        grunt{ shipPos = (250, 425), shipVel = (0, -100) }
    ],
    score = 0,
    isPaused = False
}

render :: GameState -> Picture
render (GameState
    (Player (Ship (playerX, playerY) _ shipHP _ _ _ playerMesh) _ _)
    bullets
    enemies
    score
    isPaused
    ) = if shipHP <= 0 then pictures [
        translate (-200) 50
            $ scale 0.5 0.5
            $ color white
            $ text "Game Over!",
        translate (-200) (-50)
            $ scale 0.3 0.3
            $ color yellow
            $ text ("Score: " ++ show score)
        ] else pictures [playerPic, playerHP, bulletPics, enemyPics, enemyHPs, scorePic, bulletAmount, pausedText]
    where
        playerPic = translate playerX playerY playerMesh
        playerHP = translate (playerX - 25) (playerY + 40)
            $ scale 0.1 0.1
            $ color white
            $ text ("HP: " ++ show shipHP)
        bulletPics = pictures [translate x y bulletMesh | Bullet (x, y) _ _ _ _ bulletMesh <- bullets]
        enemyPics = pictures [translate x y enemyMesh | Ship (x, y) _ _ _ _ _ enemyMesh <- enemies]
        enemyHPs = pictures [translate (x - 25) (y + 40)
            $ scale 0.1 0.1
            $ color white
            $ text ("HP: " ++ show eHP) | Ship (x, y) _ eHP _ _ _ _ <- enemies]
        scorePic =
            translate (-(windowWidthFloat/2) + 5) (windowHeightFloat/2 - 30)
            $ scale 0.2 0.2
            $ color white
            $ text ("Score: " ++ show score)
        bulletAmount = translate (-(windowWidthFloat/2) + 5) (windowHeightFloat/2 - 60)
            $ scale 0.1 0.1
            $ color yellow
            $ text ("Bullets in the scene: " ++ show (length bullets))
        pausedText = if isPaused
            then translate (-90) (-380)
                $ scale 0.2 0.2
                $ color green
                $ text "Game Paused!"
            else blank

tickScene :: Float -> GameState -> GameState
tickScene delta (GameState player bullets enemies score isPaused) =
    tickGuns delta $ GameState player' bullets' enemies' score isPaused
    where
        calcPos :: (Float, Float) -> (Float, Float) -> (Float, Float)
        calcPos (x, y) (vx, vy) = (x + delta * vx, y + delta * vy)
        pShip = playerShip player
        player' = player { playerShip = pShip { shipPos = calcPos (shipPos pShip) (shipVel pShip) } }
        bullets' = map (\b -> b { bulletPos = calcPos (bulletPos b) (bulletVel b) }) bullets
        enemies' = map (\e -> e { shipPos = calcPos (shipPos e) (shipVel e) }) enemies

tickGuns :: Float -> GameState -> GameState
tickGuns delta (GameState player bullets enemies score isPaused) =
    GameState player' bullets' enemies' score isPaused
    where
        pShip = playerShip player
        pGun = shipGun pShip
        pRate = fireRate pGun
        pCooldown = cooldown pGun + delta
        (pGun', pBullet) = if pCooldown >= pRate
            then (
                pGun { cooldown = pCooldown - pRate },
                [(bullet $ shipGun pShip) { bulletPos = shipPos pShip }]
            ) else (
                pGun { cooldown = pCooldown },
                []
            )
        eBullets = [
            Bullet {
                bulletPos = (x, y),
                bulletVel = vel,
                bulletRadius = rad,
                bulletDamage = damage,
                bulletIsFriendly = isFriendly,
                bulletMesh = mesh
            } | Ship (x, y) _ _ (Gun _ (Bullet _ vel rad damage isFriendly mesh) rate cooldown) _ _ _
            <- enemies, cooldown + delta >= rate ]
        player' = player { playerShip = pShip {
            shipGun = pGun'
        }}
        bullets' = bullets ++ pBullet ++ eBullets
        enemies' = map (
            \enemy@(Ship _ _ _ gun@(Gun _ _ rate cooldown) _ _ _) ->
                enemy { shipGun = gun { cooldown = mod' (cooldown + delta) rate } }
            ) enemies

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

updateEnemy :: [Bullet] -> Ship -> ([Bullet], Ship)
updateEnemy bullets enemy@(Ship ePos _ eHP _ eRad _ _) =
    let (hitEnemy, rest) = partition
            (\(Bullet bPos _ bRad _ fromPlayer _) ->
                fromPlayer && distance bPos ePos < eRad + bRad)
            bullets
        damage = sum (map bulletDamage hitEnemy)
        newShip = enemy { shipHP = eHP - damage }
        in (rest, newShip)

collideShips :: GameState -> GameState
collideShips (GameState player bullets enemies score isPaused) =
    GameState player' bullets' enemies' score' isPaused
    where
        pShip = playerShip player
        pPos = shipPos pShip
        pRad = shipRadius pShip
        eHits = length $ filter (\(Ship ePos _ _ _ eRad _ _) -> distance pPos ePos < pRad + eRad) enemies
        (hitPlayer, missPlayer) = partition
            (\(Bullet bPos _ bRad _ fromPlayer _) ->
                not fromPlayer &&
                distance pPos bPos < pRad + bRad)
            bullets
        playerDamage = sum $ map bulletDamage hitPlayer
        player' = player { playerShip = pShip { shipHP = shipHP pShip - eHits * 2 - playerDamage } }
        (bullets', hitEnemies) = mapAccumL updateEnemy missPlayer enemies
        (enemies', deadEnemies) = partition (\e -> shipHP e > 0) [
            enemy { shipHP = eHP - if distance pPos ePos < pRad + eRad then 5 else 0 }
            | enemy@(Ship ePos _ eHP _ eRad _ _) <- hitEnemies ]
        score' = score + sum (map shipScore deadEnemies)

despawnBullets :: GameState -> GameState
despawnBullets (GameState player bullets enemies score isPaused) =
    GameState player bullets' enemies score isPaused
    where
        bullets' = filter (\(Bullet (x, y) _ _ _ _ _) ->
            x > -(windowWidthFloat/2) &&
            x < (windowWidthFloat/2) &&
            y > -(windowHeightFloat/2) &&
            y < (windowHeightFloat/2)) bullets

update :: Float -> GameState -> GameState
update seconds_lapsed game
    | isPaused game || shipHP (playerShip $ player game) < 0 = game
    | otherwise = despawnBullets . collideShips . tickScene seconds_lapsed $ game

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'p') Down _ _) game = game { isPaused = not (isPaused game) }
handleKeys (EventKey (Char 'n') Down _ _) _ = initialState
handleKeys (EventKey (Char c) dir _ _) game = game { player = p { playerShip = ship { shipVel = velocity } } }
    where
        p = player game
        ship = playerShip p
        (vx, vy) = shipVel ship
        sign :: Float
        sign = if dir == Down then 1 else -1
        maxSpeed = playerMaxSpeed p
        velocity = case c of
            'w' -> (vx, vy + sign * maxSpeed)
            's' -> (vx, vy - sign * maxSpeed)
            'a' -> (vx - sign * maxSpeed, vy)
            'd' -> (vx + sign * maxSpeed, vy)
            _   -> (vx, vy)
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
