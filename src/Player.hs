module Player where
import Graphics.Gloss
import Window
import Graphics.Gloss.Juicy
---- / Informações da nave controlada pelo jogador

--Função carrega a imagem da nave

shipImage :: IO Picture
shipImage = 
    loadJuicyPNG "assets/player.png" >>= \maybePic ->
        case maybePic of
            Just pic -> return pic
            Nothing -> error "Não carregou a imagem"

shipColor :: Color
shipColor = white
shipSize :: (Float, Float)
shipSize = (60,48)
shipHalfWidth :: Float
shipHalfWidth = fst shipSize /2
shipHalfHeigth ::Float
shipHalfHeigth = snd shipSize /2
shootDelay :: Float
shootDelay = 0.5 


--Como a nave do jogador fica fixa no eixo y essa função guarda a posição do jogador nesse eixo
shipY :: Float
shipY =  (-halfHeight) + 30 + (snd shipSize /2)
---- \


---- Tipo PlayerInfo que guarda a posição e a velocidade da nave do jogador
data PlayerInfo = Ship {shipPosition :: Position
                        ,shipSpeed :: Float
                        }

-- Função que gera uma nave de jogador na posição inicial do jogo
generatePlayer :: PlayerInfo
generatePlayer = Ship (0,shipY) 0