module Window where
import Graphics.Gloss
import Data.Bool (bool)


type Position = (Float, Float)

--Janela do jogo:
--Largura 
width :: Int
width = 800
--Altura 
height :: Int
height = 600
--posição 
offset :: Int
offset = 100
--cor de fundo 
background :: Color
background = black

--Bordas:
halfWidth::Float
halfWidth = fromIntegral width/2
halfHeight :: Float
halfHeight = fromIntegral height/2

-- >>> halfHeight
-- 384.0
-- >>> halfWidth
-- 512.0




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


--janela do jogo
janela :: Display
janela = InWindow "Space Invaders" (width,height) (offset,offset)

renderTxt :: Color -> String -> Picture
renderTxt cor string = translate (-150) 150 $ scale 0.3 0.3 $ color cor $ text string



-- Mensagem atual a ser mostrada.
curMsg :: Int -> Bool -> Picture
curMsg 0 paused = pauseMsg paused
curMsg (-1) paused = lostMsg
curMsg _ paused = winMsg
winMsg :: Picture
winMsg = renderTxt green "You won! (r = new game)"
lostMsg :: Picture
lostMsg = renderTxt red "Git gud! (r = new game)"
pauseMsg :: Bool -> Picture
pauseMsg True = renderTxt blue "Press p to play!"
pauseMsg False = renderTxt blue ""

