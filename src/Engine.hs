module Engine where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Window
import Player
import Invaders
import Projectile
import Colisions
import System.Random


data GameAssets = GameAssets
  { shipAsset :: Picture
  }

loadAssets :: IO GameAssets
loadAssets = do
  shipA <- shipImage
  return $ GameAssets
    { shipAsset = shipA

    }



data GameMode = Menu Int| Playing | Exit deriving Eq
---- /O tipo GameState guarda os objetos do jogo que fazem parte da classe de tipos GameObject
---- esse tipo é usado para realizar o controle do estado do jogo
data GameState = GameState
    { gameMode      :: GameMode
    ,invaders       :: [InvaderInfo]
    , player        :: PlayerInfo
    , projectiles   :: [ProjectileInfo]
    , gameTimer     :: Float      
    , lastShotTime  :: Float      
    , score         :: Float
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
    draw  (Invader{invaderPos=(x,y),invaderColor = col}) = translate x y $  color col $ invader l a
        where
            (l,a) = invaderSize

    move s speed inv = case direction inv of
        Dir -> (x + speed * s,y)
        Esq -> (x - speed *s,y)
        where
            (x,y) = getPosition inv

    update sec i = i {invaderPos = moveI}
        where
            moveI = move sec speed i
            speed = 60

updateInvadersDirection :: [InvaderInfo] -> [InvaderInfo]
updateInvadersDirection invs
    | colisaoInvaderBorda invs = map (`setDirection` newDirection) invs
    | otherwise = invs
  where
    currentDirection = direction (head invs)
    newDirection = case currentDirection of
        Dir -> Esq
        Esq  -> Dir

setDirection :: InvaderInfo -> Direction -> InvaderInfo
setDirection inv newDir = inv { invaderPos=(x,y-30),direction = newDir }
    where
        (x,y) = invaderPos inv

---- /

--quadros por segundo
refreshRate :: Int
refreshRate = 60

--Estado padrão em que o jogo começa
defaultState :: GameState
defaultState = GameState
    { gameMode = Playing
    ,invaders = generateInvaders
    , player = generatePlayer
    , projectiles = []
    , gameTimer = 0
    , lastShotTime = -shootDelay   -- Permitir que o jogador possa atirar imediatamente
    , score = 0
    }


--Função que lida com os inputs do jogador
{- handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state  |gameMode state == Menu  = state
                                                            |gameMode state == Playing = state {player = (player state) {shipSpeed = -200}}
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state |gameMode state == Menu = state
                                                            |gameMode state == Playing = state {player = (player state) {shipSpeed = 200}}

handleInput (EventKey (Char 'z') Down _ _) state    | canShoot = state { projectiles = shoot (projectiles state) shipX
                                                                    ,lastShotTime = gameTimer state}
                                                    | otherwise = state
    where
        shipX = fst $ getPosition (player state)
        canShoot = (gameTimer state - lastShotTime state) >= shootDelay
handleInput _ state = state {player = (player state) {shipSpeed = 0}} -}




handleInput2::Event ->GameState -> GameState
handleInput2 (EventKey (SpecialKey KeyLeft) Down _ _) state  = case gameMode state of
    (Menu x) -> state {gameMode=updateMenu (-1) (Menu x)}
    Playing -> state {player = (player state) {shipSpeed = -200}}
handleInput2 (EventKey (SpecialKey KeyRight) Down _ _) state  = case gameMode state of
    (Menu x) -> state {gameMode=updateMenu (-1) (Menu x)} 
    Playing -> state {player = (player state) {shipSpeed = 200}}                                                    

handleInput2 (EventKey (Char 'z') Down _ _) state    | canShoot = state { projectiles = shoot (projectiles state) shipX
                                                                    ,lastShotTime = gameTimer state}
                                                | otherwise = state
    where
        shipX = fst $ getPosition (player state)
        canShoot = (gameTimer state - lastShotTime state) >= shootDelay

handleInput2 _ state = state {player = (player state) {shipSpeed = 0}}



updateMenu :: Int -> GameMode -> GameMode
updateMenu i (Menu op) | op+i > limit = Menu 0
                        |otherwise = Menu $ op+i
    where
        limit = 1

-- o Gloss fornece o tempo em segundos quando usamos a função play!!!

--a função updateObjects atualiza a posição do estado do jogo a cada segundo
updateObjetcs :: Float -> GameState -> GameState
updateObjetcs sec state = state{player=updateS,projectiles=updateP,invaders=updateI,gameTimer=updateTime,score = updateScore}
    where
        updateColision = removeColided (invaders state) (projectiles state)
        (colisionInv,colisionProj,scr) = updateColision
        updatedDirection = updateInvadersDirection colisionInv
        updateTime = gameTimer state + sec
        updateS = update sec (player state)
        updateP = map (update sec) colisionProj
        updateI = map (update sec) updatedDirection
        updateScore = score state + scr


--A função drawGame renderiza os GameObjects
drawGame :: GameState -> Picture

drawGame state = case gameMode state of
    Menu _ -> drawMenu
    Playing->pictures [drawS, drawI, drawP, pontos]
    Exit ->blank
    where
        drawP =  pictures $ map draw (projectiles state)
        drawI = pictures $ map draw (invaders state)
        drawS = draw (player state)
        pontos = drawScore(score state)
