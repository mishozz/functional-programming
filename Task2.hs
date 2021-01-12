module Task2 (grayscale,Rgb(Rgb),Image(Image)) where

import Data.Word
import Data.List
import Prelude
import System.IO
import Control.Monad

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
