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
playerInitialPos = (10, 0)
playerInitialVel = (0, 1)

data Direction = North | East | South | West | None deriving (Enum, Eq, Show, Bounded)
data GameState = Playing | Won | Lost deriving (Eq, Show) 

data PlatformGame = Game
  { 
    level :: [String],           -- Updated level layout
    initialLevel :: [String],    -- Initial level layout
    playerPos :: (Int, Int),     -- Tile coord of player
    playerVel :: (Int, Int),     -- Player velocity
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
                     renderPlayer "player" (playerPos g) g]

renderPlayer :: String -> (Int, Int) -> PlatformGame -> Picture 
renderPlayer player (x, y) game = translate x' y' $ color red $ circleSolid 7 
  where 
    (x', y') = tileToCoord (x, y)
    file = getFile player game

-- TODO: should preload images
getFile :: String -> PlatformGame -> String
getFile player game
 | otherwise = "img/" ++ player ++ step ++ ".png"
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
 | c == '.'  = translate x' y' $ blank
 | c == 'o'  = translate x' y' $ color yellow $ circleSolid 4
 | otherwise = blank
  where
    (x', y') = tileToCoord (x, y)

-- Event handling
handleKeys :: Event -> PlatformGame -> PlatformGame
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g  = g { playerVel = (1,vy) } where (vx,vy) = playerVel g
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) g  = g { playerVel = (0,vy) } where (vx,vy) = playerVel g
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g   = 
  g { playerVel = (-1,vy) } where (vx,vy) = playerVel g
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) g   = 
  g { playerVel = (0,vy) } where (vx,vy) = playerVel g
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g     = g
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g   = g
handleKeys (EventKey (Char 'p') Down _ _) g = g {paused = not (paused g)}
handleKeys _ game
 | (gameState game) /= Playing = resetGameFully game
 | otherwise = game

update :: Float -> PlatformGame -> PlatformGame
update secs game
 | (paused game)               = game
 | (gameState game) /= Playing = game
 | otherwise                   = updatePlayer game

updatePlayer g = updatePlayerPos g

canMove g = getTile x' y' g == '.'
  where (x',y') = posAdd (playerPos g) (playerVel g)

updatePlayerPos g
 | canMove g = g {playerPos = posAdd (playerPos g) (playerVel g)}
 | otherwise = g { playerVel = (0,0) }

posAdd (x,y) (x',y') = (x+x',y+y')

resetGame :: PlatformGame -> PlatformGame
resetGame g = g { playerPos = playerInitialPos, seconds = 0}

resetGameFully :: PlatformGame -> PlatformGame
resetGameFully g = resetGame $ g {gameState = Playing, level = (initialLevel g)}

initTiles = do 
  contents <- readFile "1.lvl"
  stdGen <- newStdGen
  let rows = words contents
  let initialState = Game { level = rows, initialLevel = rows, playerPos = playerInitialPos, playerVel = playerInitialVel, seconds = 0, gen = stdGen, paused = False, gameState = Playing}
  print rows
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
