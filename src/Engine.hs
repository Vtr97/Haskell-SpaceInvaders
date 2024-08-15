module Engine where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Window
import Player
import Invaders
import Projectile


data GameState = GameState
    {playerX :: Float
    ,playerSpeed :: Float
    }

refreshRate = 30

defaultState :: GameState
defaultState = GameState{playerX=0,playerSpeed=0}


handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = state { playerSpeed = -200 }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = state { playerSpeed= 200 }
handleInput _ state = state

updatePlayer :: Float -> GameState -> GameState
updatePlayer time state = state {playerX=updateShip}
    where updateShip = moveShip time (playerX state) (playerSpeed state)


