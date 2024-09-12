module Window where
import Graphics.Gloss



-- Tipo position guarda uma posição no eixo X Y
type Position = (Float, Float)

---- /Janela do jogo:
--Largura 
width :: Int
width = 800
--Altura 
height :: Int
height = 600
--posição na tela
offset :: Int
offset = 150
--cor de fundo 
background :: Color
background = black
---- \

---- /Bordas:
halfWidth::Float
halfWidth = fromIntegral width/2
halfHeight :: Float
halfHeight = fromIntegral height/2
--cor
corBorda :: Color
corBorda = red
--borda superior
bordaCima :: Picture
bordaCima = translate 0 halfHeight $ color corBorda $ rectangleSolid (fromIntegral width) 10
--borda esquerda
bordaEsq :: Picture
bordaEsq = translate (-halfWidth) 0 $ color corBorda $ rectangleSolid 10 (fromIntegral height)
--borda inferior.
bordaInf :: Picture
bordaInf = translate 0 (-halfHeight) $ color corBorda $ rectangleSolid (fromIntegral width) 10
--borda direita
bordaDir :: Picture
bordaDir= translate halfWidth 0 $ color corBorda $ rectangleSolid 10 (fromIntegral height)
bordas :: Picture
bordas = pictures [bordaEsq, bordaDir, bordaCima , bordaInf ]
---- \

---- /Cria a janela do jogo utilizando o tipo Display da biblioteca gloss
janela :: Display
janela = InWindow "Space Invaders" (width,height) (offset,offset)
---- \

---- /Desenha o menu principal
drawMenu :: Picture
drawMenu = pictures 
    [ translate (-150) 100 $ scale 0.6 0.6 $ color white $ text "Jogar"   
    , translate (-150) (-100) $ scale 0.6 0.6 $ color white $ text "Sair"
    ]
---- \

---- / Desenham a UI do jogo

-- desenha os pontos
drawScore :: Float -> Picture
drawScore score = translate x y $ scale 0.3 0.3 $ color white $ text ("Score: " ++ show (round score))
  where
    x = -halfWidth + 20  
    y = halfHeight - 50  

 -- desenha a quantidade de vidas do jogador  
drawLife :: Int -> Picture
drawLife life = translate x y $ scale 0.2 0.2 $ color white $ text ("Vidas: " ++ show life)
  where
    x = halfWidth - 150
    y = halfHeight - 50 
    
---- \