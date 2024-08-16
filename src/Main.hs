module Main where
import Graphics.Gloss
import Window
import Projectile
import Invaders
import Player (drawShip)
import Engine

main :: IO ()
main = play janela background refreshRate defaultState drawGame  handleInput updateGame 