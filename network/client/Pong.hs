import Control.Concurrent
import Data.IORef
import Data.Maybe
import Foreign.ForeignPtr
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLimg
import IO
import Network

data Sprite = Sprite { pos :: IORef (Int, Int), surface :: SDL.Surface }

data Pos = Pos { position :: IORef (Int, Int) }

data Direction = Up | Down deriving (Eq, Show, Read)

data PlayerStatus = Win | Loose deriving (Show, Read)
    
createSprite :: FilePath -> Int -> Int -> IO Sprite
createSprite filename x y = do
    p <- newIORef (x, y)
    s <- SDLimg.load filename
    return $ Sprite { pos = p, surface = s }

resX = 800
resY = 600

racketWidth = 20
racketHeight = 80
rSpeed = 16

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> IO Bool
applySurface x y src dst = SDL.blitSurface src Nothing dst offset
	where offset	=	Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }            

initScreen :: IO SDL.Surface
initScreen = do
    SDL.init [SDL.InitEverything]
    s <- SDL.setVideoMode resX resY 32 [SDL.SWSurface, SDL.DoubleBuf]
    SDL.setCaption "Pong" ""
    putStrLn "Screen initialized"
    return s
    
freeSurfaces :: [SDL.Surface] -> IO ()
freeSurfaces xs = do
    mapM_ SDL.freeSurface xs
    putStrLn "Surfaces freed"
    
draw :: Sprite -> SDL.Surface -> IO ()
draw sprite screen = do
    (x, y) <- readIORef (pos sprite)
    let spriteSurface = surface sprite
    applySurface x y spriteSurface screen
    return ()
 
{-updateBallPos :: Sprite -> BallSpeed -> Sprite -> Sprite -> IO ()    
updateBallPos ball ballSpeed racket1 racket2 = do
    (x,y) <- readIORef (pos ball)
    (r1x, r1y) <- readIORef (pos racket1)
    (r2x, r2y) <- readIORef (pos racket2)
    (vx, vy) <- readIORef (speed ballSpeed)
    (sx, sy) <- readIORef (sign ballSpeed)
    let newsx = if (x < r1x+25 && y `between` (r1y, r1y+80)) || (x > r2x -5 && y `between` (r2y, r2y+80)) {-|| x > (resX - 25) || x < 5-} then -sx else sx
    let newsy = if y > (resY - 25) || y < 5 then -sy else sy
    writeIORef (sign ballSpeed) (newsx, newsy)
    writeIORef (pos ball) (x + newsx*vx, y + newsy*vy)
    
    where between foo (a, b) = a <= foo && foo <= b
-}
    
{-updateIAPos :: Sprite -> Sprite -> IO ()
updateIAPos ball racket2 = do
    (bx, by) <- readIORef (pos ball)
    (rx, ry) <- readIORef (pos racket2)
    let newy = by --ry + (1 - (rx-bx)/resX)*(ry-by)
    writeIORef (pos racket2) (rx, newy)
-}

handleKeyEvent :: Handle -> Sprite -> SDL.Keysym -> IO ()    
handleKeyEvent serverHandle racket keysym = do
    let key = SDL.symKey keysym
    case key of
        SDL.SDLK_DOWN -> moveDown serverHandle racket
        SDL.SDLK_UP   -> moveUp   serverHandle racket
        _             -> return ()

moveUp :: Handle -> Sprite -> IO ()
moveUp serverHandle racket = do
    (x, y) <- readIORef (pos racket)
    if y-rSpeed < 0 then writeIORef (pos racket) (x, 0) else writeIORef (pos racket) (x, y - rSpeed)
    hPutStrLn serverHandle . show $ Up
    hFlush serverHandle
    
    
moveDown :: Handle -> Sprite -> IO ()
moveDown serverHandle racket = do
    (x, y) <- readIORef (pos racket)
    if y+racketHeight+rSpeed > resY then writeIORef (pos racket) (x, resY - racketHeight) else writeIORef (pos racket) (x, y+rSpeed)
    hPutStrLn serverHandle . show $ Down
    hFlush serverHandle

serverUpdates :: Handle -> Pos -> Pos -> Sprite -> IORef Bool -> IO ()
serverUpdates serverHandle (Pos ballPos) (Pos otherPos) otherRacket over = do
    str <- hGetLine serverHandle
    case (maybeRead str) :: Maybe (Int, Int) of
        Just (x, y) -> writeIORef ballPos (x, y)
        Nothing     -> do
            case (maybeRead str) :: Maybe Direction of
                Just Up -> moveUp serverHandle otherRacket
                Just Down -> moveDown serverHandle otherRacket
                Nothing -> case (maybeRead str) :: Maybe PlayerStatus of 
                    Just Win -> do 
                        putStrLn "You win"
                        writeIORef over True
                    Just Loose -> do
                        putStrLn "You loose"
                        writeIORef over True
                    Nothing -> return ()
    gameOver <- readIORef over
    if gameOver == False then serverUpdates serverHandle (Pos ballPos) (Pos otherPos) otherRacket over else return ()
                    
main :: IO ()
main = do
    screen <- initScreen
    background <- SDLimg.load "background.png"
    -- bSpeed <- newIORef (3, 3)
    -- bSign  <- newIORef (1, -1)
    racket1 <- createSprite "r.png" 10 10
    racket2 <- createSprite "r.png" 770 260
    ball    <- createSprite "ball.png" 390 290
    serverHandle <- connectTo "localhost" (PortNumber 1041)
    waitForStart serverHandle
    rnum <- waitForNumber serverHandle
    ((x1, y1), (x2, y2)) <- waitForPos serverHandle
    racket1 <- createSprite "r.png" x1 y1
    racket2 <- createSprite "r.png" x2 y2
    putStrLn "Game starts"
    over <- newIORef False
    case rnum of
        1 -> do
             forkIO $ serverUpdates serverHandle (Pos (pos ball)) (Pos (pos racket2)) racket2 over
             gameLoop serverHandle screen background racket1 racket2 ball over
        2 -> do
             forkIO $ serverUpdates serverHandle (Pos (pos ball)) (Pos (pos racket1)) racket1 over
             gameLoop serverHandle screen background racket2 racket1 ball over
    
    freeSurfaces $ screen:(map surface [racket1, racket2, ball])
    SDL.quit
    where waitForStart serverHandle = do
              str <- hGetLine serverHandle
              if str == "START" then return () else do { threadDelay 1000000 ; waitForStart serverHandle }
              
          waitForNumber serverHandle = do
              str <- hGetLine serverHandle
              let num = read str :: Int
              return num
          
          waitForPos serverHandle = do
              str1 <- hGetLine serverHandle
              str2 <- hGetLine serverHandle
              let pos1 = read str1 :: (Int, Int)
              let pos2 = read str2 :: (Int, Int)
              return (pos1, pos2)
          
          gameLoop serverHandle screen background racket1 racket2 ball over = do
              gameOver <- readIORef over
              if gameOver then return () else do
                  applySurface 0 0 background screen
                  draw racket1 screen
                  draw racket2 screen
                  draw ball    screen
                  SDL.flip screen
                  ev <- SDL.pollEvent
                  case ev of
                      SDL.Quit         -> return ()
                      SDL.KeyDown (!x) -> handleKeyEvent serverHandle racket1 x >> gameLoop serverHandle screen background racket1 racket2 ball over
                      _                -> gameLoop serverHandle screen background racket1 racket2 ball over

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads