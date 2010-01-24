import Data.IORef
import Foreign.ForeignPtr
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLimg
-- import qualified Graphics.UI.SDL.WindowManagement as WM
-- import Data.Monoid

data BallSpeed = BallSpeed { speed :: IORef (Int, Int), sign :: IORef (Int, Int) }
 
data Sprite = Sprite { pos :: IORef (Int, Int), surface :: SDL.Surface }
    
createSprite :: FilePath -> Int -> Int -> IO Sprite
createSprite filename x y = do
    p <- newIORef (x, y)
    s <- SDLimg.load filename
    return $ Sprite { pos = p, surface = s }

resX = 800
resY = 600

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
 
updateBallPos :: Sprite -> BallSpeed -> Sprite -> Sprite -> IO ()    
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
    
updateIAPos :: Sprite -> Sprite -> IO ()
updateIAPos ball racket2 = do
    (bx, by) <- readIORef (pos ball)
    (rx, ry) <- readIORef (pos racket2)
    let newy = by --ry + (1 - (rx-bx)/resX)*(ry-by)
    writeIORef (pos racket2) (rx, newy)

handleKeyEvent :: Sprite -> SDL.Keysym -> IO ()    
handleKeyEvent racket keysym = do
    let key = SDL.symKey keysym
    case key of
        SDL.SDLK_DOWN -> moveDown racket
        SDL.SDLK_UP   -> moveUp   racket
        _             -> return ()

moveUp :: Sprite -> IO ()
moveUp racket = do
    (x, y) <- readIORef (pos racket)
    writeIORef (pos racket) (x, y-16)
    
moveDown :: Sprite -> IO ()
moveDown racket = do
    (x, y) <- readIORef (pos racket)
    writeIORef (pos racket) (x, y+16)

main :: IO ()
main = do
    screen <- initScreen
    background <- SDLimg.load "background.png"
    bSpeed <- newIORef (3, 3)
    bSign  <- newIORef (1, -1)
    racket1 <- createSprite "r.png" 10 10
    racket2 <- createSprite "r.png" 770 260
    ball    <- createSprite "ball.png" 390 290
    waitClicks screen background racket1 racket2 ball (BallSpeed { speed = bSpeed, sign = bSign })
    freeSurfaces $ screen:(map surface [racket1, racket2, ball])
    SDL.quit
    -- return ()
 
    where
    waitClicks screen background racket1 racket2 ball ballSpeed = do
        updateBallPos ball ballSpeed racket1 racket2
        updateIAPos ball racket2
        applySurface 0 0 background screen
        draw racket1 screen
        draw racket2 screen
        draw ball    screen
        SDL.flip screen
        ev <- SDL.pollEvent
        case ev of
             SDL.Quit         -> return ()
             SDL.KeyDown (!x) -> handleKeyEvent racket1 x >> waitClicks screen background racket1 racket2 ball ballSpeed
             _                -> waitClicks screen background racket1 racket2 ball ballSpeed
 
untilM :: (Monad m) => (a -> Bool) -> m a -> m a
untilM f m = do
    x <- m
    if f x then return x else untilM f m 