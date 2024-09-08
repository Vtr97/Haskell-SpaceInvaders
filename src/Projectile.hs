module Projectile where
import Graphics.Gloss
import Player
import Invaders
import Window

---- /propriedades dos projeteis do jogador
playerProjectileWidth :: Float
playerProjectileWidth = 8
playerProjectileHeight :: Float
playerProjectileHeight = 20
playerProjectileSize :: (Float,Float)
playerProjectileSize = (playerProjectileWidth,playerProjectileHeight)
playerProjectileColor :: Color
playerProjectileColor = shipColor
---- \

---- /Propriedades dos projeteis dos invaders
invaderProjectileWidth :: Float
invaderProjectileWidth = 10
invaderProjectileHeight :: Float
invaderProjectileHeight = 32
invaderProjectileSize :: (Float,Float)
invaderProjectileSize = (invaderProjectileWidth,invaderProjectileHeight)
invaderProjectileColor:: Color
invaderProjectileColor = green
---- \

---- /tipo ProjectileInfo que tem dois construtores , um para projeteis do jogador e outro para projeteis dos inimigos.
--Esse tipo guarda a posição e a velocidade dos projeteis
data ProjectileInfo = PlayerProjectile
                        {projectilePos :: Position
                        ,projectileSpeed::Float}
                    | InvaderProjectile
                        {projectilePos :: Position
                        ,projectileSpeed::Float} deriving Eq
---- \

---- / Função que guarda o formato da hitbox do projetil
projectile :: Float -> Float -> Picture
projectile = rectangleSolid
---- \



---- / Função que recebe uma lista de ProjectileInfo e uma posição no eixo X e cria um Projetil nessa posição
--- Note que essa função está implementada de maneira que funciona apenas para o jogador pois ainda não implementei a funcionalidade de atirar para o inimigo
shoot :: [ProjectileInfo] -> Float ->[ProjectileInfo]
shoot proj pX = PlayerProjectile{projectilePos=(pX,shipY),projectileSpeed=300} : proj

---- \


