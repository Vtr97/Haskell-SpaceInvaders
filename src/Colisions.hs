module Colisions where
import Invaders
import Player
import Window
import Projectile


---- /Lógica da colisão com as bordas que será usada pelo nave do jogador
bordaEsqColide :: Float -> Bool
bordaEsqColide x    | x - shipHalfWidth -1 <= - halfWidth = True
                    | otherwise                        = False

bordaDirColide :: Float -> Bool
bordaDirColide x    | x + shipHalfWidth + 1 >= halfWidth    = True
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

playerColided :: Float -> [ProjectileInfo] -> Bool
playerColided shipX projs = any (playerColision shipX) projs

--- dado uma lista de invaders e projetils , retorna duplas de (projetil,invader) que colidiram
checkColision :: [InvaderInfo] -> [ProjectileInfo] -> [(InvaderInfo, ProjectileInfo)]
checkColision  invs projs = 
    [(inv, proj) | inv <- invs, proj <- projs, isPlayerProjectile proj, invaderColision inv proj]


-- Função auxiliar para verificar se o projétil é do tipo PlayerProjectile
isPlayerProjectile :: ProjectileInfo -> Bool
isPlayerProjectile (PlayerProjectile _ _) = True
isPlayerProjectile _ = False

---- função que dada uma lista de invaders e projectiles , checa colisão entre eles e então remove eles da lista , também devolve um float que indica a pontuação obtida após derrotar os invaders
removeColided ::  Position-> [InvaderInfo] -> [ProjectileInfo] -> ([InvaderInfo],[ProjectileInfo],Float,Bool)
removeColided pos invs projs = (updatedInvs,updatedProjs,updatedScore,playerColide)
    where
        colided = checkColision  invs projs
        colidedInvs = map fst colided
        colidedProjs = map snd colided
        playerColide  =  playerColided (fst pos) projs
        updatedInvs = filter (\inv-> notElem inv colidedInvs) invs
        updatedProjs = filter (\proj->notElem proj colidedProjs) projs
        updatedScore = calculateScore colidedInvs


---- função para detectar se algum invader colidiu com a borda lateral , utilizado para auziliar na movimentação dos invaders
colisaoInvaderBordaLateral :: [InvaderInfo] -> Bool
colisaoInvaderBordaLateral invs = any colisaoBorda invs
    where
        colisaoBorda inv = 
            let (x,_) = invaderPos inv in
            case direction inv of
            Dir -> x >= halfWidth
            Esq -> x <= -halfWidth

---- Função para detectar se algum invader invadiu o eixo Y do jogador , o que resulta em Game Over
colisaoInvaderPlayerY :: [InvaderInfo] -> Bool
colisaoInvaderPlayerY invs = any colisaoBorda invs
    where
        colisaoBorda inv = 
            let 
                (_,iy) = invaderPos inv
            in
                iy <= shipY       

---- função para calcular a pontuação do jogador após um projetil do jogador colidir com um invader
calculateScore :: [InvaderInfo] -> Float
calculateScore invs = foldl(\acc inv->acc+invaderScore inv) 0 invs
    where
        invaderScore inv = fromIntegral (invaderType inv * 100)
         
