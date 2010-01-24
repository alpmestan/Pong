import Control.Concurrent
import Control.Exception hiding (catch)
import Control.Monad
import Data.Maybe
import Data.IORef
import IO
import Network
import Network.Socket.ByteString

data BallSpeed = BallSpeed { speed :: IORef (Int, Int), sign :: IORef (Int, Int) }

newtype Pos = Pos { pos :: IORef (Int, Int) }

data Direction = Up | Down deriving (Eq, Show, Read)

data GameStatus = R1Win | R2Win | NotFinished deriving (Eq, Show, Read)

data PlayerStatus = Win | Loose deriving (Show)

resX = 800
resY = 600

ballWidth = 20
ballHeight = 20

racketWidth = 20
racketHeight = 80
rSpeed = 16

main = withSocketsDo $ do
    serverSock <- listenOn (PortNumber 1041) 
    acceptConnections serverSock 0 []

acceptConnections sock nbClients clientList = do
    putStrLn "Awaiting connection"
    conn@(h, host, port) <- accept sock
    print conn
    if nbClients > 1 then do
            hPutStrLn h "2 clients already connected. Sorry."
            hClose h 
            acceptConnections sock nbClients clientList        
        else do
            if nbClients == 1 then startGame (h:clientList) else acceptConnections sock (nbClients+1) (h:clientList)
        
initBall :: IO (BallSpeed, Pos)
initBall = do
    xy <- newIORef (410 - ballWidth, 310 - ballHeight)
    defaultSpeed <- newIORef (3, 3)
    defaultSign <- newIORef (1, -1)
    let ballspeed = BallSpeed { speed = defaultSpeed, sign = defaultSign }
    return (ballspeed, Pos xy)

initP1 :: IO Pos
initP1 = do
    xy <- newIORef (10, 10)
    return $ Pos xy
    
initP2 :: IO Pos
initP2 = do
    xy <- newIORef (resX - racketWidth - 10, 10)
    return $ Pos xy
    
startGame clients = do
    (ballSpeed, ballPos) <- initBall
    r1pos <- initP1
    r2pos <- initP2
    let positions = [r1pos, r2pos]
    -- mapM_ (\(Pos p) -> readIORef p >>= putStrLn . show) positions
    mapM_ (flip hPutStrLn "START") clients
    hPutStrLn (clients !! 0) "1"
    hFlush (clients !! 0)
    hPutStrLn (clients !! 1) "2"
    hFlush (clients !! 1)
    -- zipWithM_ (\h (Pos p) -> readIORef p >>= hPutStrLn h . show >> hFlush h) clients positions
    mapM_ (\(Pos po) -> do { p <- readIORef po ; hPutStrLn (clients !! 0) . show $ p }) positions
    mapM_ (\(Pos po) -> do { p <- readIORef po ; hPutStrLn (clients !! 1) . show $ p }) (reverse positions)
    forkIO $ positionUpdate (clients !! 0) r1pos (clients !! 1)
    forkIO $ positionUpdate (clients !! 1) r2pos (clients !! 0)
    handleGame ballSpeed ballPos $ zip clients positions
    mapM_ (flip hPutStrLn "END") clients
    mapM_ hClose clients

handleGame ballSpeed ballPos cls@[(h1, pos1), (h2, pos2)] = do
    putStrLn "Call to ballUpdate"
    newGameStatus <- ballUpdate ballSpeed ballPos pos1 h1 pos2 h2
    threadDelay 20000
    --threadDelay 2000000
    -- readIORef (pos pos1) >>= return . ("R1 at : " ++) . show >>= putStrLn
    -- readIORef (pos pos2) >>= return . ("R2 at : " ++) . show >>= putStrLn
    case newGameStatus of
         NotFinished -> handleGame ballSpeed ballPos cls
         x -> do
             putStrLn $ show x
             hPutStrLn h1 $ if x == R1Win then show Win else show Loose
             hPutStrLn h2 $ if x == R2Win then show Win else show Loose
             hFlush h1
             hFlush h2
    --forkIO $ positionUpdate h1 pos1
    --forkIO $ positionUpdate h2 pos2
    
ballUpdate :: BallSpeed -> Pos -> Pos -> Handle -> Pos -> Handle -> IO GameStatus
ballUpdate ballSpeed ballPos pos1 h1 pos2 h2 = do
    -- mapM_ (flip hPutStrLn "ballUpdate") [h1,h2]
    (x, y) <- readIORef (pos ballPos)
    (r1x, r1y) <- readIORef (pos pos1)
    (r2x, r2y) <- readIORef (pos pos2)
    (vx, vy) <- readIORef (speed ballSpeed)
    (sx, sy) <- readIORef (sign ballSpeed)
    let newsx = if ( x < r1x + racketWidth + 3 && y `between` (r1y, r1y + racketHeight) ) ||
                   (x > r2x - 3 && y `between` (r2y, r2y+racketHeight)) then -sx else sx
    let newsy = if y > (resY - 23) || y < 3 then -sy else sy
    writeIORef (sign ballSpeed) (newsx, newsy)
    writeIORef (pos ballPos) (x + newsx*vx, y + newsy*vy)
    sendPos ballPos [h1, h2]
    if x < 5 then return R2Win else if x > resX - 5 then return R1Win else return NotFinished
    where between foo (a, b) = a <= foo && foo <= b
          sendPos (Pos po) hs = do 
              p <- readIORef po 
              mapM_ (flip hPutStrLn (show p)) hs
              mapM_ hFlush hs
             

positionUpdate :: Handle -> Pos -> Handle -> IO ()
positionUpdate h (Pos p) otherHandle = do
    str <- hGetLine h
    (x, y) <- readIORef p
    -- putStrLn $ "- Received from " ++ show h ++ str
    let dir = maybeRead str :: Maybe Direction
    case dir of
        Nothing   -> return ()
        Just Up   -> do
            if y-rSpeed < 0 then writeIORef p (x, 0) else writeIORef p (x, y-rSpeed)
            hPutStrLn otherHandle (show Up)
            hFlush otherHandle
        Just Down -> do
            if y+racketHeight+rSpeed > resY then writeIORef p (x, resY - racketHeight) else writeIORef p (x, y+rSpeed)
            hPutStrLn otherHandle (show Down)
            hFlush otherHandle
    (nx, ny) <- readIORef p
    putStrLn $ show h ++ " from " ++ show (x, y) ++ " to " ++ show (nx, ny)
    positionUpdate h (Pos p) otherHandle
    
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads