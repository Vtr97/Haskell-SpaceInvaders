module Player where
import Graphics.Gloss
import Window

-- / Informações da nave controlada pelo jogador

shipColor :: Color
shipColor = white

shipSize :: (Float, Float)
shipSize = (60,48)

shipHalfWidth :: Float
shipHalfWidth = fst shipSize /2

shipHalfHeigth ::Float
shipHalfHeigth = snd shipSize /2

shipY :: Float
shipY =  (-halfHeight) + 30 + (snd shipSize /2)
-- /

-- / Renderização da nave
drawShip :: Float -> Picture
drawShip x = translate x shipY $ Color shipColor ship
    where
        ship = rectangleSolid l a
        (l,a) = shipSize
-- /
    

-- / Movimentação da nave e detecção de colisão com os limites da janela
moveShip :: Float -> Float -> Float -> Float
moveShip sec x vel  | detecaColisaoBorda  x && vel > 0    = x - 1
                    | detecaColisaoBorda  x && vel < 0    = x + 1
                    |otherwise                            = x'
    where
        x' = x + (vel*sec)
                


bordaEsqColide :: Float -> Bool
bordaEsqColide x    | x - shipHalfWidth <= - halfWidth = True
                    | otherwise                        = False

bordaDirColide :: Float -> Bool
bordaDirColide x    | x + shipHalfWidth >= halfWidth    = True
                    | otherwise                         = False


detecaColisaoBorda :: Float -> Bool
detecaColisaoBorda x =  bordaEsqColide x || bordaDirColide x
-- / 


