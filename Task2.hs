module Task2 (grayscale,Rgb(Rgb),Image(Image)) where

import Data.Word
import Data.List
import Prelude
import System.IO
import Control.Monad
import Data.Array

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
    | null content = []
    | otherwise = [flatten $ take w content] ++ toCorrectContent (drop w content) w 

isCorrectFormat :: [String] -> Bool 
isCorrectFormat list = head list == p3

listToPixels :: [String] -> Int -> [[Rgb]]
listToPixels list n = toCorrectContent  (transformToContent  (toWorkingList list)) n

loadImage :: String -> IO Image
loadImage path = do
    handle <- openFile path ReadMode 
    contents <- hGetContents handle
    let singleWords = words contents
    let width = singleWords!!1
    let w = read width
    let height = singleWords!!2
    let content = drop 4 singleWords
    if not $ isCorrectFormat singleWords
        then return (Image 0 0 [[]])
        else do
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
