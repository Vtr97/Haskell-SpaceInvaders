module Engine where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Window
import Player
import Invaders
import Projectile


data GameState = GameState
    {playerX :: Float
    ,player :: PlayerInfo
    ,projectiles :: [ProjectileInfo]
    }

-- Inserir os tipos que serão renderizados no jogo (jogador,invader e projetil) em uma classe de tipos
class GameObject a where
    getPosition :: a -> Position
    move :: Float -> a -> Position
    draw :: a -> Picture
    update :: Float -> a -> a
    changeSpeed :: float -> a -> a

-- Criar a instancia de gameObjetc para os tipos 
instance GameObject ProjectileInfo where
    getPosition = projectilePos 
    move sec proj = (x,y')
        where
            y' = y + vy * sec 
            (x,y) = projectilePos proj
            vy = projectileSpeed proj
    draw (PlayerProjectile (x,y) _) = translate x y $ color playerProjectileColor $ projectile pw ph
        where
            (pw,ph) = playerProjectileSize  
    draw (InvaderProjectile (x,y)_) = translate x y $ color invaderProjectileColor $ projectile iw ih
        where (iw,ih) = invaderProjectileSize
    update sec proj = proj {projectilePos= moveProj}
        where
            moveProj = move sec proj

instance GameObject PlayerInfo where
    getPosition = shipPosition
    move sec s = (x',y)
        where
            (x,y)=shipPosition s
            x' = x + vx * sec
            vx = shipSpeed s
    draw (Ship (x,y)_) = translate x y $ color shipColor $ ship l a
        where 
            (l,a) = shipSize
    update sec s = s {shipPosition = moveS}
        where
            moveS = move sec s

instance GameObject InvaderInfo where
    getPosition = invaderPos





refreshRate :: Int
refreshRate = 60

defaultState :: GameState
defaultState = GameState{player=generatePlayer,projectiles=[]}

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = state { shipSpeed  = -200 }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = state { shipSpeed= 200 }
handleInput (EventKey (Char 'z') Down _ _ ) state = state { projectiles= playerShoot(projectiles state) (playerX state) }
handleInput _ state = state{shipSpeed = 0}

-- o Gloss fornece o tempo em segundos quando chamamos a função play!!!
updateShip :: Float -> GameState -> Float
updateShip sec  state = moveShip sec (playerX state) (playerSpeed state)

updateProjectile :: Float -> ProjectileInfo -> ProjectileInfo
updateProjectile sec project = project {projectilePos=moveproj}
    where 
        (x,y) = projectilePos project
        moveproj = moveProjectile sec (x,y) (projectileSpeed project)


updateProjectiles :: Float -> GameState -> Projectiles
updateProjectiles sec state = map (updateProjectile sec) (projectiles state)



updateGame :: Float -> GameState -> GameState
updateGame sec state = state    {playerX= updateShip sec state
                                ,projectiles= updateProjectiles sec state}





drawGame :: GameState -> Picture
drawGame state = pictures [drawShip (playerX state),drawInvaders generateInvaders, drawProjectiles (projectiles state)]