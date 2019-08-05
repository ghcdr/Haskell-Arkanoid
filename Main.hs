module Main where
import Graphics.Gloss
import System.IO.Unsafe
import System.Random
import System.IO
import Data.Char
import Graphics.Gloss.Game
import qualified Graphics.Gloss.Game as Gloss

blockSprite :: Picture
blockSprite = png "img/block.png"

ballSprite :: Picture
ballSprite = png "img/ball.png"

platformSprite :: Picture
platformSprite = png "img/platform.png"

wallSprite :: Picture
wallSprite = png "img/sidewall.png"

pickupSprite :: Picture
pickupSprite = scale 0.03 0.03 (png "img/block.png")

h :: Float
h = 450

w :: Float
w = 800

xRes :: Int
xRes = 1600

yRes :: Int
yRes = 900

time :: Int
time = 30

timef::Float 
timef = 30

hud :: Int -> Picture
hud v = text (show v)

pointsBanner :: String 
pointsBanner = "Score:"

pRows:: Int
pRows = 15

pCols:: Int
pCols = 6

aux :: Int
aux = 0

data Vec = V (Float, Float)
	deriving(Eq, Show)

data Game = Game {pPlatform :: Vec, vPlatform :: Vec, pBall :: Vec, pBallOld :: Vec,vBall :: Vec, pickupsCommonList :: [(Float, Float, Picture)], totalPoints :: Int, points :: Int, run :: Bool, message :: String}
	deriving(Eq, Show)

newGame :: Game
newGame = Game{pPlatform = V (0, -410), vPlatform = V (0, 0), pBall = V (0, 0), pBallOld = V (0, 200), vBall = V (200, 300), pickupsCommonList = getPickups 15 6 (-650) (100) 80 pickupSprite, totalPoints = (pRows * pCols), points = 0, run = True, message = ""}

main :: IO ()
main = Gloss.play (InWindow "Arkanoid" (xRes, yRes) (50, 50)) white time newGame draw handle [updateBallVelocity, updateBallPosition, checkBoundsCollisions, collidePlat, accPlatform, movePlatform, pickupBallReaction, collectPickup, gameOver]
	where
    draw (Game {pPlatform = V (x, y), pBall = V (u, v), pickupsCommonList = plist, points = i, message = msn}) = pictures [ pictures (map trans plist), translate (-500) (-100) (scale 0.7 0.7 (text msn)), translate 100 (-100) (scale 1 1 (text pointsBanner)), translate 530 (-100) (scale 1 1 (hud (i))), translate u v (scale 0.07 0.07 ballSprite), translate x y (scale 0.2 0.04 platformSprite), translate 760 0 (scale 0.1 0.8 wallSprite), translate (-760) 0 (scale 0.1 0.8 wallSprite)] 

    handle (EventKey (Char 'a') Down _ _) game = game {vPlatform = moveLeft 70 (vPlatform game)}
    handle (EventKey (Char 'd') Down _ _) game = game {vPlatform = moveRight 70 (vPlatform game)}
    handle _event game = game

moveLeft :: Float -> Vec -> Vec
moveLeft speed (V (x, y)) = V ((speed * (-1)) + x, 0 + y)

moveRight :: Float -> Vec -> Vec
moveRight speed (V (x, y)) = V ((speed * 1) + x, 0 + y) 

-- Steps
updateBallVelocity _ game = game {vBall = updateBallVel 1 (vBall game) (pBallOld game)}

updateBallPosition _ game = game {pBallOld = (pBall game), pBall = updateBallPos (1/timef) (pBall game) (vBall game)}

checkBoundsCollisions _ game = game {vBall = collideBounds (vBall game) (pBall game)}

accPlatform _ game = game {vPlatform = dampVelocity 0.9 (vPlatform game)}

movePlatform _ game = game {pPlatform = updatePos 0.167 (pPlatform game) (vPlatform game)}

collidePlat _ game = game {vBall = collidePlatform (vBall game) (pBall game) (pPlatform game)}

pickupBallReaction _ game = game {vBall = (reflectPickupBallCollision (pBall game) (vBall game) (pickupsCommonList game))}

collectPickup _ game = game {pickupsCommonList = pickupCollide (pBall game) (pickupsCommonList game), points = ((totalPoints game) - (length (pickupsCommonList game)))}

gameOver _ game = game {run = testGameOver (pBall game) (pickupsCommonList game) (run game), message = getMessage (run game)}


--
-- aux
reflectPickupBallCollision :: Vec -> Vec -> [(Float, Float, Picture)] -> Vec
reflectPickupBallCollision (V (x, y)) (V (i, j)) [] = (V (i, j))
reflectPickupBallCollision (V (x, y)) (V (i, j)) ((u, v, pic):l) = let length = sqrt (((x - u)*(x - u)) + ((y - v) * (y - v)))
										   						   	in if (length < 35)
										   	  			   			  then (rotateit (V ((sqrt ((i * i) + (j * j))) * ((u - x) / length), (sqrt ((i * i) + (j * j))) * (((v - y)/length)))) (180))
										   	               			  else (reflectPickupBallCollision (V (x, y)) (V (i, j)) l)

getMessage :: Bool -> String
getMessage b 
	| b  = ""
	| otherwise = "Game Over"

testGameOver :: Vec -> [(Float, Float, Picture)] -> Bool -> Bool
testGameOver (V (x, y)) l state
	| state == False = False
	| (l == []) || (y <= (-430)) = False
	| otherwise = True

pickupCollide :: Vec -> [(Float, Float, Picture)] -> [(Float, Float, Picture)]
pickupCollide (V (x, y)) [] = []
pickupCollide (V (x, y)) ((u, v, pic):l) = let length = sqrt (((x - u)*(x - u)) + ((y - v) * (y - v)))
										   in if length < 35
										   	  then pickupCollide (V (x, y)) l
										   	  else [(u, v, pic)] ++ (pickupCollide (V (x, y)) l)
										   	  

--rows/col/offsetx/offsety/spacing/image
getPickups :: Float -> Float -> Float -> Float -> Float -> Picture -> [(Float, Float, Picture)]
getPickups i 0 x y spc pic = []
getPickups i j x y spc pic = (pickapickup i x spc y pic) ++ (getPickups i (j - 1) x (y + spc) spc pic)

--count/x/xoffset/yfixedval/pic/
pickapickup :: Float -> Float -> Float-> Float -> Picture -> [(Float, Float, Picture)]
pickapickup 0 a b c d = []
pickapickup count x xoffset y pic = [(x + (xoffset * count), y, pic)] ++ (pickapickup (count - 1) x xoffset y pic)

trans::(Float, Float, Picture) -> Picture
trans (a,b,c) = translate a b c

updatePos :: Float -> Vec -> Vec -> Vec
updatePos t (V (x, y)) (V (u, v)) = V ((u * t) + x, (v * t) + y)

dampVelocity :: Float -> Vec -> Vec 
dampVelocity d (V (x, y)) = (V (x * d, y * d))

updateBallVel :: Float -> Vec -> Vec -> Vec
updateBallVel damp (V (x, y)) (V (u, v)) = V ((x)*damp, (y)*damp)

updateBallPos :: Float -> Vec -> Vec -> Vec
updateBallPos step (V (x, y)) (V (u, v)) = V (x + (u*step), y + (v*step))

collideBounds :: Vec -> Vec -> Vec
collideBounds (V (x, y)) (V (u, v))
	| u > 700 || u < -700 = (V (-x, y))
	| v > 430 || v < -430 = (V (x, -y))
	| otherwise = (V (x, y))

collidePlatform :: Vec -> Vec -> Vec -> Vec
collidePlatform (V (v1, v2)) (V (x, y)) (V (u, v)) 
	| x > (u - 90) && x < (u + 90) && y > (v - 30) && y < (v + 30) =  (V (v1, -v2))
	| otherwise = (V (v1, v2))

-- Geometry functions
dot :: Vec -> Vec -> Float
dot (V (x, y)) (V (u, v)) = (u * x) + (v * y)

angle :: Vec -> Vec -> Float
angle a b = atan(dot a b)

rotateit :: Vec -> Float -> Vec
rotateit (V (x, y)) alpha = V (((cos alpha) * x) - ((sin alpha) * y), ((sin alpha) * x) + ((cos alpha) * y))

len :: Vec -> Vec -> Float
len (V (x, y)) (V (u, v)) = sqrt (((x - u)*(x - u)) + ((y - v) * (y - v)))

