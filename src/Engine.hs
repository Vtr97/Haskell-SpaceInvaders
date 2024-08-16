module Engine where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Window
import Player
import Invaders
import Projectile

data GameState = GameState
    {playerX :: Float
    ,playerSpeed :: Float
    ,projectiles :: Projectiles
    }

refreshRate :: Int
refreshRate = 60

defaultState :: GameState
defaultState = GameState{playerX=0,playerSpeed=0,projectiles=[]}

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = state { playerSpeed = -200 }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = state { playerSpeed= 200 }
handleInput (EventKey (Char 'z') Down _ _ ) state = state { projectiles= playerShoot(projectiles state) (playerX state) }
handleInput _ state = state{playerSpeed = 0}

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