module Invaders where
import Graphics.Gloss
import Window 

type InvaderType = Int

-- / Propriedades dos invasores
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

-- Informações dos invaders
data InvaderInfo = Invader
    {invaderPos :: Position -- Coordenada do invasor
    ,invaderColor :: Color -- cor do invasor
    ,invaderType :: InvaderType} 

type Invaders = [InvaderInfo]

hasInvaders :: Invaders -> Bool
hasInvaders = null

numberInvaders :: Invaders -> Int
numberInvaders = length

drawInvaders :: Invaders -> Picture
drawInvaders invaders = pictures $ map drawInvader invaders
    where
        drawInvader (Invader{invaderPos=(x,y),invaderColor = color}) = translate x y $ Color color invader
        invader = rectangleSolid l a
        (l,a) = invaderSize



-- / Invasores tem tamanho (40,30) e vamos distribuir 5 linhas de 11 invasores com um espaçamento em x = 10  e
-- espaçamento em y = 20 portanto eles ocuparão (40+10)*11 = 550 pixels em x e (30+20)*5 = 250 pixels em Y
-- Portanto em relação ao centro (0,0) metade dos invaders ocuparão a faixa em x de [-275,275] pois ficarão centralizados em relação a x e 
-- para que os invaders da ultima linha fiquem ? pixels para ? do eixo Y eles ocuparão [50,300]



generateInvaders :: Invaders
generateInvaders = [generateInvader l c| l <-[0..4], c <- [0..10]]
generateInvader :: Int -> Int -> InvaderInfo
generateInvader linha coluna = Invader
                                {invaderPos = (xPosition coluna, yPosition linha)
                                ,invaderColor = selectColor linha
                                ,invaderType = linha
                                }
selectColor :: Int -> Color
selectColor  l  | l == 0 || l == 1 = light blue
                | l == 2 = green
                | l == 3 = cyan
                | l == 4 = orange
        

xPosition :: Int -> Float
xPosition coluna = (-275) + fromIntegral(50*coluna)

yPosition :: Int -> Float
yPosition linha = 50 + fromIntegral(50*linha)

