module Main where
import Graphics.Gloss
import Window
import Projectile
import Invaders
import Player 
import Engine
import Colisions


---- A função Main utiliza a função play do Gloss para renderizar a janela do jogo e então desenha os objetos do jogo e os atualiza a cada segundo
main :: IO ()
main = play janela background refreshRate defaultState drawGame  handleInput updateObjetcs