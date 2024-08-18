module Colisions where
import Invaders
import Player
import Window
import Projectile


---- /Lógica da colisão com as bordas que será usada pelo nave do jogador
bordaEsqColide :: Float -> Bool
bordaEsqColide x    | x - shipHalfWidth <= - halfWidth = True
                    | otherwise                        = False

bordaDirColide :: Float -> Bool
bordaDirColide x    | x + shipHalfWidth >= halfWidth    = True
                    | otherwise                         = False

detecaColisaoBorda :: Float -> Bool
detecaColisaoBorda x =  bordaEsqColide x || bordaDirColide x
---- \

------ / Lógica das colisões dos projeteis
--overlap detecta se dado hitbox (x,x') ela sobrepõe com outra hitbox (z,z') 
overlap :: Position -> Position -> Bool
overlap (x,x') (z,z') = x <= z &&  x' >=z ||  x <= z' && x' >=z'

-- utiliza overlap para detectar o overlap da hitbox de um projetil com um invader o que indica uma colisão
invaderColision :: InvaderInfo -> ProjectileInfo -> Bool
invaderColision _ (InvaderProjectile{}) = False
invaderColision (Invader{invaderPos=(x,y)}) (PlayerProjectile{projectilePos=(w,z)}) = overlapX && overlapY
    where
        overlapX = overlap (x-ihalfWidth,x+ihalfWidth) (w-(playerProjectileWidth/2),w+(playerProjectileWidth/2))
        overlapY = overlap (y-ihalfHeight,y+ihalfHeight) (z-(playerProjectileHeight/2),z+(playerProjectileHeight/2))

-- detecta colisão de um projetil com o jogador
playerColision :: Float -> ProjectileInfo -> Bool
playerColision _ (PlayerProjectile{}) = False
playerColision shipX (InvaderProjectile{projectilePos=(x,y)}) = overlapX && overlapY
    where
        overlapX = overlap (shipX-shipHalfWidth,shipX+shipHalfWidth) (x-(invaderProjectileWidth/2),x+(invaderProjectileWidth/2))
        overlapY = overlap (shipY-shipHalfHeigth,shipY+shipHalfHeigth) (y-(invaderProjectileHeight/2),y+(invaderProjectileHeight/2))


--- dado uma lista de invaders e projetils , retorna duplas de (projetil,invader) que colidiram
checkColision :: [InvaderInfo] -> [ProjectileInfo] -> [(InvaderInfo,ProjectileInfo)]
checkColision invs projs = [(inv,proj)| inv <- invs , proj <- projs , invaderColision inv proj]





