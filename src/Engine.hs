module Engine where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Window
import Player
import Invaders
import Projectile
import Colisions
import System.Random
import Control.Monad.State


---- O tipo gameAssets guarda as imagens do jogo
data GameAssets = GameAssets
  { shipAsset :: Picture
  , greenAsset :: Picture
  , redAsset :: Picture
  , yellowAsset :: Picture
  }

---- Load Assets é usada no contexto de IO () do main para carregar os assets do jogo
loadAssets :: IO GameAssets
loadAssets = do
  shipA <- shipImage
  greenI <- grennInvader
  redI <- redInvader
  yellowI <- yellowInvader
  return $ GameAssets
    { shipAsset = shipA
    ,greenAsset = greenI
    ,redAsset = redI
    ,yellowAsset = yellowI
    }


--- o tipó GameMode é usado para definir os diferentes estados do jogo
data GameMode = Menu Int| Playing | Exit deriving Eq


---- /O tipo GameState guarda os objetos do jogo que fazem parte da classe de tipos GameObject e outras informações relevantes de controle
---- esse tipo é usado para realizar o controle do estado do jogo
data GameState = GameState
    { gameMode      :: GameMode
    ,invaders       :: [InvaderInfo]
    , player        :: PlayerInfo
    , projectiles   :: [ProjectileInfo]
    , gameTimer     :: Float      
    , lastShotTime  :: Float      
    , score         :: Float
    ,playerLife     :: Int
    }

---- \

--- o tipo jogo define a monada de estado do GameState
type Jogo a = State GameState a



---- / Os tipos que serão renderizados no jogo e tem capacidade de se mover foram inseridos em uma classe de tipos GameObject 
class GameObject a where
    getPosition :: a -> Position
    move :: Float -> Float -> a -> Position
    draw :: GameAssets -> a -> Picture
    update :: Float -> a -> a
---- \

---- / Criar a instancia de gameObjetc para cada um dos tipos 
instance GameObject ProjectileInfo where
    getPosition = projectilePos
    move sec s proj = (x,y')
        where
            y' = y + s * sec
            (x,y) = projectilePos proj
    draw _ (PlayerProjectile (x,y) _)  = translate x y $ color playerProjectileColor $ projectile pw ph
        where
            (pw,ph) = playerProjectileSize
    draw _ (InvaderProjectile (x,y)_) = translate x y $ color invaderProjectileColor $ projectile iw ih
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



    draw asset (Ship (x,y)_)  = translate x y $ shipAsset asset
    update sec s = s {shipPosition = moveS}
        where
            moveS = move sec speed s
            speed = shipSpeed s

instance GameObject InvaderInfo where
    getPosition = invaderPos
    draw asset (Invader{invaderPos=(x,y),invaderType=t}) = 
        case t of
            0 -> translate x y $  greenAsset asset
            1 -> translate x y $  greenAsset asset
            2 -> translate x y $  redAsset asset
            3 -> translate x y $  redAsset asset
            4 -> translate x y $  yellowAsset asset

    move s speed inv = case direction inv of
        Dir -> (x + speed * s,y)
        Esq -> (x - speed *s,y)
        where
            (x,y) = getPosition inv

    update sec i = i {invaderPos = moveI}
        where
            moveI = move sec speed i
            speed = 60

--- Essa função é usada para detectar se algum invader colidiu com a borda , se sim ela inverte a direção de todos os invaders
updateInvadersDirection :: [InvaderInfo] -> [InvaderInfo]
updateInvadersDirection invs
    | colisaoInvaderBorda invs = map (setDirection newDirection) invs
    | otherwise = invs
  where
    currentDirection = direction (head invs)
    newDirection = case currentDirection of
        Dir -> Esq
        Esq  -> Dir

---- A função setDirection muda a direção de um invader
setDirection ::Direction -> InvaderInfo -> InvaderInfo
setDirection newDir inv = inv { invaderPos=(x,y-30),direction = newDir }
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
    , lastShotTime = -shootDelay   
    , score = 0
    , playerLife = 3
    }




handleInputState :: Event -> Jogo GameState
handleInputState (EventKey (SpecialKey KeyLeft) Down _ _)= do
    gs <- get 
    case gameMode gs of
        (Menu x) -> put gs {gameMode = updateMenu (-1) (Menu x)}
        Playing  -> put gs {player = (player gs) {shipSpeed = -200}}
    return gs

handleInputState (EventKey (SpecialKey KeyRight) Down _ _) = do
    gs <- get
    case gameMode gs of
        (Menu x) -> put gs {gameMode=updateMenu (-1) (Menu x)} 
        Playing -> put gs {player = (player gs) {shipSpeed = 200}}
    return gs

handleInputState (EventKey (Char 'z') Down _ _) = do
    gs <- get
    let shipX = fst $ getPosition (player gs)
        canShoot = (gameTimer gs - lastShotTime gs) >= shootDelay
        in
            if canShoot then do
                put gs  { projectiles = shoot (projectiles gs) shipX
                        ,lastShotTime = gameTimer gs}
                return gs
            else
                return gs

handleInputState _  = do
    gs <- get
    put gs {player = (player gs) {shipSpeed = 0}}
    return gs
   
 

updateMenu :: Int -> GameMode -> GameMode
updateMenu i (Menu op) | op+i > limit = Menu 0
                        |otherwise = Menu $ op+i
    where
        limit = 1


updateObjectsState :: Float -> Jogo GameState
updateObjectsState sec = state atualizaObjetos
    where
        atualizaObjetos :: GameState -> (GameState,GameState)
        atualizaObjetos gameState = let 
            updateColision = removeColided (invaders gameState) (projectiles gameState)
            (colisionInv,colisionProj,scr) = updateColision
            updatedDirection = updateInvadersDirection colisionInv
            updateTime = gameTimer gameState + sec
            updateS = update sec (player gameState)
            updateP = map (update sec) colisionProj
            updateI = map (update sec) updatedDirection
            updateScore = score gameState + scr in
            (gameState,gameState{player=updateS,projectiles=updateP,invaders=updateI,gameTimer=updateTime,score = updateScore})


drawGameState :: GameAssets ->Jogo Picture
drawGameState assets = state desenhaJogo
    where
        desenhaJogo :: GameState -> (Picture,GameState)
        desenhaJogo gameState = let
            drawP =  pictures $ map (draw assets) (projectiles gameState)
            drawI = pictures $ map (draw assets) (invaders gameState)
            drawS = draw assets (player gameState)
            pontos = drawScore(score gameState)
            vida = drawLife(playerLife gameState) in
                case gameMode gameState of
                    Menu _ -> (drawMenu,gameState)
                    Playing->(pictures [drawS, drawI, drawP, pontos,vida],gameState)
                    Exit ->(blank,gameState)