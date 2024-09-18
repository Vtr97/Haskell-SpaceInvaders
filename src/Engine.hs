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
data GameMode = Menu Int| Playing | GameOver Int | Victory Int deriving Eq 


---- /O tipo GameState guarda os objetos do jogo que fazem parte da classe de tipos GameObject e outras informações relevantes de controle
---- esse tipo é usado para realizar o controle do estado do jogo
data GameState = GameState
    { gameMode      :: GameMode
    ,invaders       :: [InvaderInfo]
    , player        :: PlayerInfo
    , projectiles   :: [ProjectileInfo]
    , gameTimer     :: Float
    , lastShipShotTime  :: Float
    , lastInvaderShotTime :: Float
    , score         :: Float
    ,playerLife     :: Int
    ,aleatorios     :: [Int]
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
    | colisaoInvaderBordaLateral invs = map (setDirection newDirection) invs
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
--


--- /  Essa função eu adaptei do slide do exercício do jogo da velha, ela cria uma lista aleatoria infinita de IDs de invader
randomInvaderList:: IO [Int]
randomInvaderList = do
    gen <- getStdGen
    let rns = randomRs (0 ,54) gen in
        return rns

---- /Estado padrão em que o jogo começa (No caso ele começa no Menu)
defaultState :: [Int]->GameState
defaultState als = GameState
    { gameMode = Menu 1
    ,invaders = generateInvaders
    , player = generatePlayer
    , projectiles = []
    , gameTimer = 0
    , lastShipShotTime = -shipShootDelay
    ,lastInvaderShotTime = 0
    , score = 0
    , playerLife = 3
    ,aleatorios = als
    }
---- \

---- /Estado padrão do jogo quando o jogador seleciona Play no menu ou restart no gameOver , basicamente reseta o jogo para o estado inicial de gameplay
defaultPlayState :: [Int]->GameState
defaultPlayState als = GameState
    { gameMode = Playing
    ,invaders = generateInvaders
    , player = generatePlayer
    , projectiles = []
    , gameTimer = 0
    , lastShipShotTime = -shipShootDelay
    , lastInvaderShotTime = (-0.5)
    , score = 0
    , playerLife = 3
    ,aleatorios=als
    }
----- \


---- / HandleInvaderShoot é a monada State que lida com o processo de selecionar um inimigo aleatório da última coluna para atirar , ela retorna () pois apenas atualiza o estado
handleInvaderShoot :: Jogo ()
handleInvaderShoot = do
    gs <- get
    let 
        canShoot = (gameTimer gs - lastInvaderShotTime gs) >= invaderShotDelay
        (shootingInvader,listaAtualizada) = getShooterInvader (aleatorios gs) (lastInvaderInColumn $ invaders gs)
        pos = invaderPos shootingInvader
    if canShoot 
        then do
            put gs { projectiles = invaderShoot (projectiles gs) pos
                    , lastInvaderShotTime = gameTimer gs , aleatorios = listaAtualizada }
    else return ()
---- \


---- / Monada state que lida com a condição de vitória do jogo , retorna () pois apenas atualiza o estado
handleVictory :: Jogo ()
handleVictory = do
    gs <- get
    let
        invs = invaders gs
    case invs of
        [] -> put gs {gameMode = Victory 1}
        _ -> return ()

---- \

---- / Essa função recebe um Event (aperto de teclas) e retorna uma monada State com o jogo atualizado
handleInputState :: Event -> Jogo ()
handleInputState (EventKey (SpecialKey KeyLeft) Down _ _)= do
    gs <- get
    case gameMode gs of
            Menu _  -> return ()
            Playing  -> put gs {player = (player gs) {shipSpeed = -200}}
            GameOver _ -> return ()
            Victory _  -> return ()
            
handleInputState (EventKey (SpecialKey KeyRight) Down _ _) = do
    gs <- get
    case gameMode gs of
            Menu _ ->  return ()
            Playing ->  put gs {player = (player gs) {shipSpeed = 200}}
            GameOver _ -> return ()
            Victory _ -> return ()

handleInputState (EventKey (SpecialKey KeyUp) Down _ _)= do
    gs <- get
    case gameMode gs of
            Menu _ -> updateMenuState 1
            Playing  -> return ()
            GameOver _ -> updateMenuState 1
            Victory _ -> updateMenuState 1
            
handleInputState (EventKey (SpecialKey KeyDown) Down _ _) = do
    gs <- get
    case gameMode gs of
            Menu _ -> updateMenuState (-1)
            Playing  -> return ()
            GameOver _ -> updateMenuState (-1)
            Victory _ -> updateMenuState (-1)
            
handleInputState (EventKey (Char 'z') Down _ _) = do
    modify $ \gs ->
        let shiPos = getPosition (player gs)
            canShoot = (gameTimer gs - lastShipShotTime gs) >= shipShootDelay
            in
                case gameMode gs of
                Playing -> if canShoot then do
                                gs  { projectiles = playerShoot (projectiles gs) shiPos
                                    ,lastShipShotTime = gameTimer gs}
                            else
                                gs
                Menu x ->   if    x == 1 then
                                    defaultPlayState $ aleatorios gs
                            else gs
                GameOver x ->   if x == 1 then
                                    defaultPlayState $ aleatorios gs
                                else gs
                Victory x ->    if x == 1 then
                                    defaultPlayState $ aleatorios gs
                                else gs

handleInputState _  = do
    modify $ \gs ->
        gs {player = (player gs) {shipSpeed = 0}}
---- \

---- / essa monada State lida com o GameOver , ela detecta se o jogador tem 0 vidas ou os inimigos atingiram o limite do eixo Y e muda o estado do jogo para GameOver
handlePlayerDeath :: Jogo ()
handlePlayerDeath = do
    gs <- get
    let lives = playerLife gs
        invs = invaders gs
    case gameMode gs of
            Menu _ ->  return ()
            Playing ->  if lives <= 0 || colisaoInvaderPlayerY invs then
                        put gs{gameMode = GameOver 1}
                     else
                        return ()
            GameOver _ -> return ()
            Victory _ -> return ()
---- \


---- / Essa monada lida com a atualização do menu do jogo
updateMenuState :: Int -> Jogo ()
updateMenuState i = do
    gs <- get
    case gameMode gs of
        Menu op ->     put gs{gameMode=updateMenu Menu op}
        GameOver op -> put gs{gameMode=updateMenu GameOver op}
        Victory op -> put gs{gameMode=updateMenu Victory op}
        _ -> return ()
    where
        updateMenu gm op    | op+i > limit = gm 1
                            | op+i <= 0 = gm limit
                            |otherwise = gm $ op+i
        limit = 1 
---- \



---- / Essa monada de estado recebe o tempo do jogo em segundos fornecido pela função play do Gloss e então atualiza o estado do jogo
updateObjectsState :: Float -> Jogo ()
updateObjectsState sec = do
    gs <- get
    let
        updateColision = removeColided shipPos (invaders gs) (projectiles gs)
        (colisionInv, colisionProj, scr, b) = updateColision
        updatedDirection = updateInvadersDirection colisionInv
        updateTime = gameTimer gs + sec
        updateShip = update sec (player gs)
        updateProjects = map (update sec) colisionProj
        updateInvaders = map (update sec) updatedDirection
        updateScore = score gs + scr
        ship = player gs
        shipPos = shipPosition ship
    case gameMode gs of
        Menu _ -> return ()
        Playing -> do
            if b
                then do
                    put $ gs { playerLife = playerLife gs - 1
                             , player=generatePlayer
                             ,projectiles=[] }
                else do
                    put $ gs { player = updateShip
                             , projectiles = updateProjects
                             , invaders = updateInvaders
                             , gameTimer = updateTime
                             , score = updateScore }
            handleInvaderShoot >> handlePlayerDeath >> handleVictory
        GameOver _ -> return ()
        Victory _ -> return ()
        
---- \


---- / Essa função recebe os Assets do jogo e então devolve uma State monad com o tipo de retorno Picture , ela é utilizada para desenhar as imagens do jogo
drawGameState :: GameAssets ->Jogo Picture
drawGameState assets = do
    gs <- get
    let
        drawP =  pictures $ map (draw assets) (projectiles gs)
        drawI = pictures $ map (draw assets) (invaders gs)
        drawS = draw assets (player gs)
        pontos = drawScore (score gs)
        vida = drawLife (playerLife gs)
        in
            case gameMode gs of
                Menu x -> return$ drawMenu x
                Playing -> return (pictures [drawS, drawI, drawP, pontos,vida])
                GameOver x -> return $ drawGameOver x
                Victory x -> return $ drawVictory x
------ \

