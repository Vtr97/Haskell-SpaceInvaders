module Invaders where
import Graphics.Gloss
import Window
import System.Random
import Graphics.Gloss.Juicy
import Data.List 
import Data.Function(on)

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

invaderShotDelay :: Float
invaderShotDelay = 1.6

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
    ,invaderLinha :: Int
    ,invaderColuna :: Int
    ,invaderId :: Int
    ,direction :: Direction}  deriving (Eq)
type InvaderType = Int
---- \

instance Show InvaderInfo where
    show inv = "Linha: " ++ show (invaderLinha inv)

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
                                , invaderLinha = linha
                                , invaderColuna = coluna
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


--- Função auxiliar que compara os invaders por coluna e caso sejam iguais compara por linhas
compareInvader :: InvaderInfo -> InvaderInfo -> Ordering
compareInvader inv1 inv2 =
    case compare (invaderColuna inv1) (invaderColuna inv2) of
        EQ -> compare (invaderLinha inv2) (invaderLinha inv1)
        ord -> ord

-- Função para encontrar o invasor mais baixo em cada coluna que serão utilizados para escolher o invader que irá atirar
-- Encontrei a solução aqui https://stackoverflow.com/questions/12398458/how-to-group-similar-items-in-a-list-using-haskell
lastInvaderInColumn :: [InvaderInfo] -> [InvaderInfo]
lastInvaderInColumn invaders =
    map last $ groupBy ((==) `on` invaderColuna) sortedInvaders
  where
    sortedInvaders = sortBy compareInvader invaders

---- / Função que utiliza uma lista aleatória infinita e uma lista de invaders e busca um invader para atirar e também consome a lista. A lista de InvaderInfo passada é o resultado de lastInvaderInColumn
getShooterInvader :: [Int] -> [InvaderInfo] ->  (InvaderInfo,[Int])
getShooterInvader (x:xs) invs =
    case find (\inv -> invaderId inv == x) invs of
        Just inv -> (inv,xs)
        Nothing -> getShooterInvader xs invs
----- \

