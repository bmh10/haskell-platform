module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Game as GG
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO  
import System.Random
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 5
width = 420 -- 28 * 15
height = 465 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100
tileSize = 15
maxTileHoriz = 27
window = InWindow "Platform" (width, height) (offset, offset)
background = black

data Direction = North | East | South | West | None deriving (Enum, Eq, Show, Bounded)
data GameState = Playing | Won | Lost deriving (Eq, Show) 

data PlatformGame = Game
  { 
    level :: [String],           -- Updated level layout
    initialLevel :: [String],    -- Initial level layout
    pacmanPos :: (Int, Int),     -- Tile coord of pacman
    pacmanDir :: Direction,      -- Pacman's direction of travel
    seconds :: Float,            -- Game timer
    gen :: StdGen,               -- Random number generator
    paused :: Bool,              -- Paused or not
    gameState :: GameState       -- State of the game
  } deriving Show 

-- Tile functions
getTile :: Int -> Int -> PlatformGame -> Char
getTile x y g = (level g) !! y !! x

setTile :: Int -> Int -> Char -> PlatformGame -> PlatformGame
setTile x y c g = g {level = updatedLevel}
  where updatedLevel = setAtIdx y (setAtIdx x c ((level g) !! y)) (level g)

onTick :: PlatformGame -> Bool -> Int -> a -> a -> a 
onTick g c t a b = if (c && (mod (round (seconds g)) t) == 0) then a else b

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> (Float, Float) 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

setAtIdx :: Int -> a -> [a] -> [a]
setAtIdx idx val xs = take idx xs ++ [val] ++ drop (idx+1) xs

-- Rendering
render :: PlatformGame -> Picture 
render g = pictures [renderLevel g, 
                     renderPlayer "pacman" (pacmanPos g) (pacmanDir g) Normal g]

renderPlayer :: String -> (Int, Int) -> Direction -> PlayerState -> PlatformGame -> Picture 
renderPlayer player (x, y) dir state game = translate x' y' $ GG.png file
  where 
    (x', y') = tileToCoord (x, y)
    file = getFile player dir state game

-- TODO: should preload images
getFile :: String -> Direction -> PlayerState -> PlatformGame -> String
getFile player dir state game
 | otherwise = "img/" ++ player ++ show dir ++ step ++ ".png"
  where 
    step = onTick game True 2 "1" "2"

renderLevel :: PlatformGame -> Picture
renderLevel game = renderLines (level game) 0

renderLines :: [String] -> Int -> Picture
renderLines [] _ = blank
renderLines (l:ls) y = pictures [renderLine l 0 y, renderLines ls (y+1)]

renderLine :: String -> Int -> Int -> Picture
renderLine [] _ _      = blank
renderLine (t:ts) x y  = pictures [renderTile t x y, renderLine ts (x+1) y]

renderTile :: Char -> Int -> Int -> Picture
renderTile c x y
 | c == 'x'  = translate x' y' $ color blue $ rectangleSolid (tileSize-1) (tileSize-1)
 | c == '+'  = translate x' y' $ color white $ rectangleSolid (tileSize-1) 2
 | c == '.'  = translate x' y' $ color yellow $ circleSolid 2
 | c == 'o'  = translate x' y' $ color yellow $ circleSolid 4
 | otherwise = blank
  where
    (x', y') = tileToCoord (x, y)

-- Event handling
handleKeys :: Event -> PlatformGame -> PlatformGame
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g  = setPacmanDir East g
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g   = setPacmanDir West g
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g     = setPacmanDir North g
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g   = setPacmanDir South g
handleKeys (EventKey (Char 'p') Down _ _) g = g {paused = not (paused g)}
handleKeys _ game
 | (gameState game) /= Playing = resetGameFully game
 | otherwise = game

update :: Float -> PlatformGame -> PlatformGame
update secs game
 | (paused game)               = game
 | (gameState game) /= Playing = game
 | otherwise                   = game


resetGame :: PlatformGame -> PlatformGame
resetGame g = g { pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, ghostPos = [redGhostInitialPos, blueGhostInitialPos, yellowGhostInitialPos, pinkGhostInitialPos], ghostDir = replicate 4 ghostInitialDir, ghostState = replicate 4 CenterZone, seconds = 0, pacmanNextDir = None, scaredTimer = 0, countdownTimer = 3}

resetGameFully :: PlatformGame -> PlatformGame
resetGameFully g = resetGame $ g {gameState = Playing, lives = pacmanInitialLives, score = 0, level = (initialLevel g), coinCount = countCoins (initialLevel g)}

initTiles = do 
  contents <- readFile "1.lvl"
  stdGen <- newStdGen
  let rows = words contents
  let initialState = Game { level = rows, initialLevel = rows, pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, ghostPos = [redGhostInitialPos, blueGhostInitialPos, yellowGhostInitialPos, pinkGhostInitialPos], ghostDir = replicate 4 ghostInitialDir, ghostState = replicate 4 CenterZone, score = 0, seconds = 0, lives = pacmanInitialLives, pacmanNextDir = None, gen = stdGen, scaredTimer = 0, paused = False, countdownTimer = 3, gameState = Playing, coinCount = countCoins rows, ghostEatenCount = 0 }
  print rows
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
