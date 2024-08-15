module Main where
import Graphics.Gloss
import Window
import Projectile
import Invaders
import Player (drawShip)
import Engine

main :: IO ()
myInvaders :: Invaders
myInvaders = [ Invader (100, 150) red 3
             , Invader (200, 150) green 1
             , Invader (300, 150) blue 1
             ]
main = play janela background 30 