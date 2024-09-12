module Invaders where
import Graphics.Gloss
import Window
import System.Random
import Graphics.Gloss.Juicy


---- / Funções que carregam as imagens dos invaders
grennInvader :: IO Picture
grennInvader = 
    loadJuicyPNG "assets/green.png" >>= \maybePic ->
        case maybePic of
            Just pic -> return pic
            Nothing -> error "Não carregou a imagem"

redInvader :: IO Picture
redInvader = 
    loadJuicyPNG "assets/red.png" >>= \maybePic ->
        case maybePic of
            Just pic -> return pic
            Nothing -> error "Não carregou a imagem"

yellowInvader :: IO Picture
yellowInvader = 
    loadJuicyPNG "assets/yellow.png" >>= \maybePic ->
        case maybePic of
            Just pic -> return pic
            Nothing -> error "Não carregou a imagem"

---- /

---- / Propriedades dos invasores
-- Invasores por fileira
invaderRow :: Int
invaderRow = 11

invaderLine ::Int
invaderLine = 5

totalInvaders :: Int
totalInvaders = invaderRow * invaderLine
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

---- \


---- / direction é usado para auxiliar no movimento dos invaders
data Direction = Esq | Dir

instance Eq  Direction where 
    Esq == Esq = True
    Dir == Dir = True
    _ == _ = False

---- \

---- / Tipo InvaderInfo que guarda as informações de um invasor
data InvaderInfo = Invader
    {invaderPos :: Position -- Coordenada do invasor
    ,invaderType :: InvaderType
    ,invaderId :: Int
    ,direction :: Direction}  deriving (Eq)
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
                                ,invaderType  = selecType linha
                                ,invaderId = genId
                                ,direction = Dir
                                }
    where 
        genId = linha * 11 + coluna
        selecType l     | l == 0 || l == 1 = 1
                        | otherwise = l


-- Função que dado um invader e uma lista de invaders , remove esse invader da lista utilizando seu invaderID
killInvader :: InvaderInfo -> [InvaderInfo] -> [InvaderInfo]
killInvader (Invader{invaderId=i}) = filter checkId
    where
        checkId inv = i /= invaderId inv
---- / Essas funções definem a logica da distribuição dos invaders na janela do jogo
xPosition :: Int -> Float
xPosition coluna = (-275) + fromIntegral (50*coluna)
yPosition :: Int -> Float
yPosition linha = 50 + fromIntegral (50*linha)
---- \

---- / Função auxiliar que inverte a direção 
invertDirection :: Direction -> Direction
invertDirection Esq = Dir
invertDirection Dir = Esq
---- \


chooseRandomInvader :: [ InvaderInfo] -> Maybe InvaderInfo
chooseRandomInvader[] = Nothing



