module Task2 (grayscale,floodFill,imgToP3Format,toCorrectContent,transfromToWord8,isCorrectFileFormat,Rgb(Rgb),Image(Image)) where

import Data.Word ( Word8 )
import Data.Char ( isDigit )
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Eq,Show,Read)

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Eq,Show,Read)

redMultiplier :: Float 
redMultiplier = 0.30

greenMultiplier :: Float 
greenMultiplier = 0.59

blueMultiplier :: Float 
blueMultiplier = 0.11

p3 :: [Char]
p3 = "P3"

maxValueColor :: [Char]
maxValueColor = "255"

word8ToFloat :: Word8 -> Float
word8ToFloat  = fromIntegral 

intToWord8 :: Int  -> Word8
intToWord8  = fromIntegral 

stringToWord8 :: String -> Word8
stringToWord8 = intToWord8 . read

changeColor :: Word8 -> Float -> Float
changeColor color multiplier =  word8ToFloat color*multiplier

transfromToWord8 :: Rgb -> Word8
transfromToWord8 (Rgb red green blue) = intToWord8 $ round (changeColor red redMultiplier + changeColor green greenMultiplier + changeColor blue blueMultiplier)

greyscaleRgb :: Rgb -> Rgb
greyscaleRgb rgb = Rgb (transfromToWord8 rgb) (transfromToWord8 rgb) (transfromToWord8 rgb)

grayscale :: Image -> Image
grayscale (Image w h content) = Image w h (map (map greyscaleRgb) content)

rgbToP3 :: Rgb -> [Char]
rgbToP3 (Rgb r g b) = unwords [show r, show g, show b]

p3ToRgb :: [String] -> [Rgb]
p3ToRgb str = [Rgb (stringToWord8 $ str!!0)  (stringToWord8 $ str!!1) (stringToWord8 $ str!!2)]

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

contentToP3 :: [Rgb] -> [[Char]]
contentToP3 = map rgbToP3

imgToP3Format:: Image -> [[Char]]
imgToP3Format (Image w h content) = [p3] ++[unwords [show w,show h]] ++ [maxValueColor] ++ contentToP3 (flatten content)

saveImage :: FilePath -> Image -> IO()
saveImage filePath img = writeFile filePath (unlines (imgToP3Format img))

toWorkingList :: [String] -> [[String]]
toWorkingList list
    | null list = []
    | otherwise =  [take 3 list] ++ toWorkingList (drop 3 list)

transformToContent :: [[String]] -> [[Rgb]]
transformToContent  = map p3ToRgb 

toCorrectContent :: [[Rgb]] -> Int -> [[Rgb]]
toCorrectContent content w 
    | w == 0 = [[]]
    | null content = []
    | otherwise = [flatten $ take w content] ++ toCorrectContent (drop w content) w 

isNumber :: String -> Bool
isNumber ""  = False
isNumber "." = False
isNumber xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

isCorrectFileFormat :: [String] -> Bool 
isCorrectFileFormat list = length list >= 4
                            && head list == p3 
                            && isNumber (list!!1)
                            && isNumber (list!!2)
                            && length (drop 4 list) ==  (read $ list!!1) * (read $ list!!2) * 3 -- w*h*3 because Rgb has 3 colors

listToPixels :: [String] -> Int -> [[Rgb]]
listToPixels list n = toCorrectContent  (transformToContent  (toWorkingList list)) n

loadImage :: String -> IO Image
loadImage path = do
    handle <- openFile path ReadMode 
    contents <- hGetContents handle
    let singleWords = words contents
    if not $ isCorrectFileFormat singleWords
        then return (Image 0 0 [[]])
        else do
                let width = singleWords!!1
                let w = read width
                let height = singleWords!!2
                let content = drop 4 singleWords
                return (Image (read width) (read height) (listToPixels content w))

inBounds :: Int -> Int -> Int -> Int -> Bool
inBounds x y xBorder yBorder = x >= 0 && x <= xBorder && y >= 0 && y <= yBorder

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

changePixelAt :: Int -> Int -> a -> [[a]] -> [[a]]
changePixelAt row col x xs =
    let row_to_replace_in = xs !! row
        modified_row = replaceNth col x row_to_replace_in
    in replaceNth row modified_row xs

getPixelAt :: [[Rgb]] -> Int -> Int -> Rgb
getPixelAt content n m = content !! n !! m

floodFillHelper :: [[Rgb]] -> Int -> Int -> Rgb -> Rgb -> Int -> Int -> [[Rgb]]
floodFillHelper content x y startRgb replacement xBorder yBorder 
    | ((not $ inBounds x y xBorder yBorder) || getPixelAt content x y /= startRgb || startRgb == replacement) = content
    | otherwise = 
        contentNorth
        where content' = changePixelAt x y replacement content
              contentEast = floodFillHelper content' (x+1) y startRgb replacement xBorder yBorder
              contentWest = floodFillHelper contentEast (x-1) y startRgb replacement xBorder yBorder
              contentSouth = floodFillHelper contentWest x (y+1) startRgb replacement xBorder yBorder
              contentNorth = floodFillHelper contentSouth x (y-1) startRgb replacement xBorder yBorder

floodFill :: Rgb -> Int -> Int -> Image -> Image
floodFill color x y (Image w h content) 
    | not (inBounds x y (h-1) (w-1))  = error "Invalid cordinates x and y"
    | content == [[]] = error  "Unable to floodFill empty Image content"
    | otherwise = Image w h (floodFillHelper content x y (getPixelAt content x y) color (h-1) (w-1))

getAt :: [[a]] -> Int -> Int -> a
getAt grid n m = grid !! n !! m

getGx :: [[Float]] -> Float 
getGx grid = (getAt grid 0 0) * (-1) + (getAt grid 0 1) * 0 + (getAt grid 0 2) * 1 
                + (getAt grid 1 0) * (-2) + (getAt grid 1 1) * 0 + (getAt grid 1 2) * 2
                + (getAt grid 2 0) * (-1) + (getAt grid 2 1) * 0 + (getAt grid 2 2) * 1

getGy :: [[Float]] -> Float 
getGy grid = (getAt grid 0 0) * (-1) + (getAt grid 0 1) * (-2) + (getAt grid 0 2) * (-1)
                + (getAt grid 1 0) * 0 + (getAt grid 1 1) * 0 + (getAt grid 1 2) * 0
                + (getAt grid 2 0) * 1 + (getAt grid 2 1) * 2 + (getAt grid 2 2) * 1

norm :: Float -> Float -> Float 
norm x y = sqrt (x*x + y*y)

isDownLeftDiagonal :: Int -> Int -> Int -> Int -> Bool
isDownLeftDiagonal x y xBorder yBorder = x == xBorder && y == 0

isDownRightDiagonal :: Int -> Int -> Int -> Int -> Bool
isDownRightDiagonal x y xBorder yBorder = x == xBorder && y == yBorder

isTopRightDiagonal :: Int -> Int -> Int -> Int -> Bool
isTopRightDiagonal x y xBorder yBorder = x == 0 && y == yBorder

isTopLeftDiagonal :: Int -> Int -> Int -> Int -> Bool
isTopLeftDiagonal x y xBorder yBorder = x == 0 && y == 0

getVector1 :: Int -> Int -> Image -> [Float] 
getVector1 x y (Image w h grid)
    | isTopLeftDiagonal x y xBorder yBorder = [word8ToFloat $ red $ getPixelAt grid (x+1) (y+1),word8ToFloat $ red $ getPixelAt grid (x+1) y, word8ToFloat $ red $ getPixelAt grid (x+1) (y+1)]
    | isTopRightDiagonal x y xBorder yBorder = [word8ToFloat $ red $ getPixelAt grid (x+1) (y-1),word8ToFloat $ red $ getPixelAt grid (x+1) y, word8ToFloat $ red $ getPixelAt grid (x+1) (y-1)]
    | not (inBounds (x-1) y xBorder yBorder) = [word8ToFloat $ red $ getPixelAt grid (x+1) (y+1),word8ToFloat $ red $ getPixelAt grid (x+1) y, word8ToFloat $ red $ getPixelAt grid (x+1) (y-1)]
    | otherwise = [word8ToFloat $ red $ getPixelAt grid (x-1) (y-1),word8ToFloat $ red $ getPixelAt grid (x-1) y, word8ToFloat $ red $ getPixelAt grid (x-1) (y+1)]
    where xBorder = h-1
          yBorder = y-1

getVector2 :: Int -> Int -> Image -> [Float]
getVector2 x y (Image w h grid) 
    | not (inBounds x (y-1) (h-1) (w-1)) = [word8ToFloat $ red $ getPixelAt grid x (y+1),word8ToFloat $ red $ getPixelAt grid x y, word8ToFloat $ red $ getPixelAt grid x (y+1)]
    | not (inBounds x (y+1) (h-1) (w-1)) = [word8ToFloat $ red $ getPixelAt grid x (y-1),word8ToFloat $ red $ getPixelAt grid x y, word8ToFloat $ red $ getPixelAt grid x (y-1)]
    | otherwise = [word8ToFloat $ red $ getPixelAt grid x (y-1),word8ToFloat $ red $ getPixelAt grid x y, word8ToFloat $ red $ getPixelAt grid x (y+1)]

getVector3 :: Int -> Int -> Image -> [Float] 
getVector3 x y (Image w h grid)
    | isDownLeftDiagonal x y xBorder yBorder = [word8ToFloat $ red $ getPixelAt grid (x-1) (y+1),word8ToFloat $ red $ getPixelAt grid (x-1) y, word8ToFloat $ red $ getPixelAt grid (x-1) (y+1)]
    | isDownRightDiagonal x y xBorder yBorder = [word8ToFloat $ red $ getPixelAt grid (x-1) (y-1),word8ToFloat $ red $ getPixelAt grid (x-1) y, word8ToFloat $ red $ getPixelAt grid (x-1) (y-1)]
    | not (inBounds (x+1) y xBorder yBorder) = [word8ToFloat $ red $ getPixelAt grid (x-1) (y+1),word8ToFloat $ red $ getPixelAt grid (x-1) y, word8ToFloat $ red $ getPixelAt grid (x-1) (y-1)]
    | otherwise = [word8ToFloat $ red $ getPixelAt grid (x+1) (y-1),word8ToFloat $ red $ getPixelAt grid (x+1) y, word8ToFloat $ red $ getPixelAt grid (x+1) (y+1)]
    where xBorder = h-1
          yBorder = y-1

getMatrix ::Int -> Int -> Image -> [[Float]]
getMatrix x y img 
    | width img == 1 && height img == 1 = [[word8ToFloat $ red $ getPixelAt (content img) 0 0]]
    | otherwise = [getVector1 x y img,getVector2 x y img, getVector3 x y img]

getCorrectPixel :: [[Float]] -> Rgb
getCorrectPixel grid = 
    let notClampedVal = round (norm (getGx grid) (getGy grid))
        val = if notClampedVal > 255 then 255 else notClampedVal
    in Rgb val val val

isPixelGrayscale :: Rgb -> Bool 
isPixelGrayscale pixel = red pixel == green pixel && green pixel == blue pixel

isGrayscale :: [Rgb] -> Bool 
isGrayscale list
    | null list = True
    | otherwise = isPixelGrayscale (head list) && isGrayscale (tail list)

for :: [a] -> (a -> b) -> [b]
for = flip map

generateContent :: Image -> [[Rgb]]
generateContent img = concat nested_list
  where xBorder = height img - 1
        yBorder = width img - 1
        nested_list =
          for [0..xBorder] (\i ->
            for [0..yBorder] (\j ->
             [getCorrectPixel (getMatrix i j img)]
            )
          )

edgeDetect :: Image -> Image
edgeDetect img
    | not $ isGrayscale $ flatten (content img) = error "given image is not greyscale"
    | otherwise = Image (width img) (height img) (toCorrectContent (generateContent img) (width img))
