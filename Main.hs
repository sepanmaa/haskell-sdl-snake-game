{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random
import Control.Concurrent (threadDelay)
import Control.Monad.State
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Data.Word (Word8)


data Direction = LeftDir | RightDir | UpDir | DownDir deriving Eq


data GameData = GameData { snake :: [(CInt, CInt)]
                         , food  :: [(CInt, CInt)]
                         , dir   :: Direction
                         , running :: Bool
                         , score :: Int
                         }


screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)


tileSize :: CInt
tileSize = 20


io :: IO a -> StateT GameData IO a
io = liftIO


key :: SDL.KeyboardEventData -> SDL.Scancode
key event = SDL.keysymScancode (SDL.keyboardEventKeysym event)


tile :: (CInt, CInt) -> Maybe (SDL.Rectangle CInt)
tile (x, y) = Just $ SDL.Rectangle (P $ V2 (tileSize*x) (tileSize*y)) (V2 tileSize tileSize)


drawTiles :: SDL.Renderer -> [(CInt, CInt)] -> (Word8, Word8, Word8) -> IO ()
drawTiles renderer points (r, g, b) = do
  SDL.rendererDrawColor renderer $= V4 r g b 255
  mapM_ (\p -> SDL.fillRect renderer $ tile p) points


drawSnake :: SDL.Renderer -> StateT GameData IO ()
drawSnake renderer = do
  game <- get
  io $ drawTiles renderer [(head $ snake game)] (0, 255, 0)
  io $ drawTiles renderer (tail $ snake game) (0, 200, 0)


drawFood :: SDL.Renderer -> StateT GameData IO ()
drawFood renderer = do
  game <- get
  io $ drawTiles renderer (food game) (255, 0, 0)


newPosition :: Direction -> (CInt, CInt) -> (CInt, CInt)
newPosition dir (x, y) =
  case dir of
    LeftDir ->
      (x-1, y)
    RightDir ->
      (x+1, y)
    UpDir ->
      (x, y-1)
    DownDir ->
      (x, y+1)


moveSnake :: StateT GameData IO ()
moveSnake = do
  game <- get
  let (x', y') = newPosition (dir game) (head $ snake game)
      hitWall = x' < 0 || y' < 0 || x'*tileSize >= screenWidth || y'*tileSize >= screenHeight
      hitSnake = any ((==) (x', y')) (tail $ snake game)
      consumed = filter ((==) (x',y')) $ food game
      food' = filter ((/=) (x',y')) $ food game
      snakeTail = if (length consumed) > 0 then snake game else init $ snake game
      hit = hitWall || hitSnake
  put game { snake = if not hit
                     then (x', y') : snakeTail
                     else snake game
           , running = not hit && running game
           , food = food'
           , score = if (length consumed) > 0 then score game + 1 else score game
           }
  return ()


makeFood :: Int -> StateT GameData IO ()
makeFood n = do
  game <- get
  g <- io $ newStdGen
  let randomPoints = zip (randomRs (0, 39) g) (randomRs (0, 29) g)
      ps = take n $ filter (not . (`elem` snake game)) randomPoints
      food' = food game
  put $ game { food = if food' == [] then ps else food' }
  return ()


getControls :: [SDL.Event] -> StateT GameData IO ()
getControls es = do
  game <- get
  let currentDir = dir game
      setDir d = game { dir = d }
  put $ foldr (\e s ->
           case SDL.eventPayload e of
             SDL.KeyboardEvent ke ->
               case key ke of
                 SDL.ScancodeEscape ->
                   s { running = False }
                 SDL.ScancodeLeft ->
                   if currentDir /= RightDir then setDir LeftDir else s
                 SDL.ScancodeRight ->
                   if currentDir /= LeftDir then setDir RightDir else s
                 SDL.ScancodeUp ->
                   if currentDir /= DownDir then setDir UpDir else s
                 SDL.ScancodeDown ->
                   if currentDir /= UpDir then setDir DownDir else s
                 _ -> s
             _ ->
               s
           ) game es
  return ()

  
gameLoop :: SDL.Renderer -> StateT GameData IO Int
gameLoop renderer = do
  events <- SDL.pollEvents
  getControls events
  moveSnake
  makeFood 3
  io $ SDL.rendererDrawColor renderer $= V4 0 0 0 maxBound
  io $ SDL.clear renderer
  drawSnake renderer
  drawFood renderer
  io $ SDL.present renderer
  io $ threadDelay 100000
  game <- get
  if not (running game) then return $ score game else gameLoop renderer


startGame :: GameData
startGame = GameData { snake   = reverse $ map (\x -> (x, 5)) [5..10]
                     , food    = []
                     , dir     = RightDir
                     , running = True
                     , score   = 0
                     }

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow
            "Snake"
            SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  renderer <- SDL.createRenderer
    window
    (-1)
    SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedRenderer
                       , SDL.rendererTargetTexture = False }
  runStateT
    (gameLoop renderer >>= io . putStrLn . (++) "Game over! Score: " . show)
    startGame
  SDL.destroyWindow window
  SDL.quit
