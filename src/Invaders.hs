module Invaders where
import Graphics.Gloss
import Window


---- / Propriedades dos invasores
-- Invasores por fileira
invaderRow :: Int
invaderRow = 11
--Tamanho dos invasores
invaderSize :: (Float,Float)
invaderSize = (30, 24)
ihalfWidth :: Float
ihalfWidth = (1 + fst  invaderSize) / 2
ihalfHeight :: Float
ihalfHeight = (1 + snd invaderSize) / 2
--Função que define o formato do hitbox dos inimigos
invader :: Float -> Float -> Picture
invader = rectangleSolid

---- / Tipo InvaderInfo que guarda as informações de um invasor
data InvaderInfo = Invader
    {invaderPos :: Position -- Coordenada do invasor
    ,invaderColor :: Color -- cor do invasor
    ,invaderType :: InvaderType
    ,invaderId :: Int}  deriving (Eq)
type InvaderType = Int
---- \

---- / Invasores tem tamanho (30,24) e vamos distribuir 5 linhas de 11 invasores com um espaçamento em x = 20  e
-- espaçamento em y = 26 portanto eles ocuparão (30+20)*11 = 550 pixels em x e (24+26)*5 = 250 pixels em Y
-- Portanto em relação ao centro (0,0) metade dos invaders ocuparão a faixa em x de [-275,275] pois ficarão centralizados em relação ao eixo x e 
-- para que os invaders da ultima linha fiquem 50 pixels para cima do centro no eixo Y eles ocuparão [50,300]
---- \


---- Função que gera uma lista de invaders seguindo um conjunto de regras de : Distribuição na tela , cor , e tipo
generateInvaders :: [InvaderInfo]
generateInvaders = [generateInvader l c| l <-[0..4], c <- [0..10]]
generateInvader :: Int -> Int -> InvaderInfo
generateInvader linha coluna = Invader
                                {invaderPos = (xPosition coluna, yPosition linha)
                                ,invaderColor = selectColor linha
                                ,invaderType  = selecType linha
                                ,invaderId = genId
                                }
    where 
        genId = linha * 11 + coluna
        selecType l     | l == 0 || l == 1 = 0
                        | otherwise = l

--regra que seleciona a cor do invader baseado na linha em que ele está
selectColor :: Int -> Color
selectColor  l  | l == 0 || l == 1 = light blue
                | l == 2 = green
                | l == 3 = cyan
                | l == 4 = orange

-- Função que dado um invader e uma lista de invaders , remove esse invader da lista utilizando seu invaderID
killInvader :: InvaderInfo -> [InvaderInfo] -> [InvaderInfo]
killInvader (Invader _ _ _ i) = filter checkId
    where
        checkId inv= i /= invaderId inv

---- / Essas funções definem a logica da distribuição dos invaders na janela do jogo
xPosition :: Int -> Float
xPosition coluna = (-275) + fromIntegral (50*coluna)
yPosition :: Int -> Float
yPosition linha = 50 + fromIntegral (50*linha)
---- \

