{-# LANGUAGE InstanceSigs #-}
module Engine where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Window
import Player
import Invaders
import Projectile
import Colisions

---- /O tipo GameState guarda os objetos do jogo que fazem parte da classe de tipos GameObject
---- esse tipo é usado para realizar o controle do estado do jogo
data GameState = GameState
    { invaders      :: [InvaderInfo]
    , player        :: PlayerInfo
    , projectiles   :: [ProjectileInfo]
    , gameTimer     :: Float      -- Tempo total do jogo
    , lastShotTime  :: Float      -- Tempo em que o último tiro foi disparado
    }

---- \

---- / Os tipos que serão renderizados no jogo e tem capacidade de se mover foram inseridos em uma classe de tipos GameObject 
class GameObject a where
    getPosition :: a -> Position
    move :: Float -> Float -> a -> Position
    draw :: a -> Picture
    update :: Float -> a -> a
---- \

---- / Criar a instancia de gameObjetc para cada um dos tipos 
instance GameObject ProjectileInfo where
    getPosition = projectilePos
    move sec s proj = (x,y')
        where
            y' = y + s * sec
            (x,y) = projectilePos proj
    draw (PlayerProjectile (x,y) _) = translate x y $ color playerProjectileColor $ projectile pw ph
        where
            (pw,ph) = playerProjectileSize
    draw (InvaderProjectile (x,y)_) = translate x y $ color invaderProjectileColor $ projectile iw ih
        where (iw,ih) = invaderProjectileSize
    update sec proj = proj {projectilePos= moveProj}
        where
            moveProj = move sec speed proj
            speed = projectileSpeed proj

instance GameObject PlayerInfo where
    getPosition = shipPosition
    move sec speed p    | detecaColisaoBorda  x && speed > 0    = (x-5,y)
                        | detecaColisaoBorda  x && speed < 0    = (x+5,y)
                        | otherwise                           = (x',y)
        where
            (x,y)=shipPosition p
            x' = x + speed * sec



    draw (Ship (x,y)_) = translate x y $ color shipColor $ ship l a
        where
            (l,a) = shipSize
    update sec s = s {shipPosition = moveS}
        where
            moveS = move sec speed s
            speed = shipSpeed s

instance GameObject InvaderInfo where
    getPosition = invaderPos
    draw :: InvaderInfo -> Picture
    draw  (Invader{invaderPos=(x,y),invaderColor = col}) = translate x y $  color col $ invader l a
        where
            (l,a) = invaderSize
---- Falta definir como os invaders irão se movimentar então por hora eles ficam parados     
    move s speed inv = if x > rl || x < ll
        then (x ,y-down)
        else(x+speed*s,y)
        where
            (x,y) = getPosition inv
            ll = -350
            rl = 350
            down = 0.5

    update sec i = i {invaderPos = moveI}
        where
            moveI = move sec speed i
            speed = 30
---- /

--quadros por segundo
refreshRate :: Int
refreshRate = 60

--Estado padrão em que o jogo começa
defaultState :: GameState
defaultState = GameState
    { invaders = generateInvaders
    , player = generatePlayer
    , projectiles = []
    , gameTimer = 0
    , lastShotTime = -shootDelay   -- Permitir que o jogador possa atirar imediatamente
    }


--Função que lida com os inputs do jogador
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = state {player = (player state) {shipSpeed = -200}}
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = state {player = (player state) {shipSpeed = 200}}
handleInput (EventKey (Char 'z') Down _ _) state
    | canShoot = state { projectiles = shoot (projectiles state) shipX
                       , lastShotTime = gameTimer state
                       }
    | otherwise = state
    where
        shipX = fst $ getPosition (player state)
        canShoot = (gameTimer state - lastShotTime state) >= shootDelay
handleInput _ state = state {player = (player state) {shipSpeed = 0}}



-- o Gloss fornece o tempo em segundos quando usamos a função play!!!

--a função updateObjects atualiza a posição do estado do jogo a cada segundo
updateObjetcs :: Float -> GameState -> GameState
updateObjetcs sec state = state{player=updateS,projectiles=updateP,invaders=updateI,gameTimer=updateTime}
    where
        updateTime = gameTimer state + sec
        updateS = update sec (player state)
        updateP = map (update sec) colisionProj
        updateI = map (update sec) colisionInv
        updateColision = removeColided (invaders state) (projectiles state)
        (colisionInv,colisionProj) = updateColision


--A função drawGame renderiza os GameObjects
drawGame :: GameState -> Picture
drawGame state = pictures [drawS, drawI, drawP]
    where
        drawP =  pictures $ map draw (projectiles state)
        drawI = pictures $ map draw (invaders state)
        drawS = draw (player state)