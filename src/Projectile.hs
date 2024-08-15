module Projectile where
import Graphics.Gloss
import Player
import Invaders
import Window

--propriedades dos projeteis do jogador
playerProjectileWidth :: Float
playerProjectileWidth = 8
playerProjectileHeight :: Float
playerProjectileHeight =20


playerProjectileSize :: (Float,Float)
playerProjectileSize = (playerProjectileWidth,playerProjectileHeight)


--cor do projetil
playerProjectileColor :: Color
playerProjectileColor = shipColor
--Propriedades dos projeteis dos invaders
invaderProjectileWidth :: Float
invaderProjectileWidth = 10
invaderProjectileHeight :: Float
invaderProjectileHeight = 32

invaderProjectileSize :: (Float,Float)
invaderProjectileSize = (invaderProjectileWidth,invaderProjectileHeight)
invaderProjectileColor:: Color
invaderProjectileColor = green



data ProjectileInfo = PlayerProjectile
                        {projectilePos :: Position}
                    | InvaderProjectile
                        {projectilePos :: Position}

type Projectiles = [ProjectileInfo]

drawProjectiles :: Projectiles -> Picture
drawProjectiles projectiles = pictures $ map drawProjectile projectiles
projectile :: Float -> Float -> Picture
projectile = rectangleSolid
drawProjectile :: ProjectileInfo -> Picture
drawProjectile (PlayerProjectile{projectilePos=(x,y)}) = translate x y $ color playerProjectileColor $ projectile pw ph
    where
        (pw,ph)=playerProjectileSize
drawProjectile (InvaderProjectile {projectilePos=(x,y)}) = translate x y $ color invaderProjectileColor $ projectile iw ih
    where
        (iw,ih)=invaderProjectileSize

moveProjectile :: Float -> Position -> Float -> Position
moveProjectile sec (x,y) vy = (x,y')
    where
        y'= y + vy*sec


------ / Colisoes de projeteis
--overlap detecta se dado hitbox (x,x') ela sobrepõe com outra hitbox (z,z') 
overlap :: Position -> Position -> Bool
overlap (x,x') (z,z') = x <= z &&  x' >=z ||  x <= z' && x' >=z'



invaderColision :: InvaderInfo -> ProjectileInfo -> Bool
invaderColision _ (InvaderProjectile{}) = False
invaderColision (Invader{invaderPos=(x,y)}) (PlayerProjectile{projectilePos=(w,z)}) = overlapX && overlapY
    where
        overlapX = overlap (x-ihalfWidth,x+ihalfWidth) (w-(playerProjectileWidth/2),w+(playerProjectileWidth/2))
        overlapY = overlap (y-ihalfHeight,y+ihalfHeight) (z-(playerProjectileHeight/2),z+(playerProjectileHeight/2))


playerColision :: Float -> ProjectileInfo -> Bool
playerColision _ (PlayerProjectile{}) = False
playerColision shipX (InvaderProjectile{projectilePos=(x,y)}) = overlapX && overlapY
    where
        overlapX = overlap (shipX-shipHalfWidth,shipX+shipHalfWidth) (x-(invaderProjectileWidth/2),x+(invaderProjectileWidth/2))
        overlapY = overlap (shipY-shipHalfHeigth,shipY+shipHalfHeigth) (y-(invaderProjectileHeight/2),y+(invaderProjectileHeight/2))


----- / "Explodir" invaders e jogador 
