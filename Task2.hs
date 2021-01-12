import Data.Word
import Prelude
import System.IO

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show,Read)

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Show,Read)

redMultiplier :: Float 
redMultiplier = 0.3

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

changeColor :: Word8 -> Float -> Int
changeColor color multiplier = fromIntegral (round (word8ToFloat color*multiplier))

transfromToWord8 :: Rgb -> Word8
transfromToWord8 (Rgb red green blue) = intToWord8 (changeColor red redMultiplier + changeColor green greenMultiplier + changeColor blue blueMultiplier)

greyscaleRgb :: Rgb -> Rgb
greyscaleRgb rgb = Rgb (transfromToWord8 rgb) (transfromToWord8 rgb) (transfromToWord8 rgb)

grayscale :: Image -> Image
grayscale (Image w h content) = Image w h (map (map greyscaleRgb) content)

rgbToP3 :: Rgb -> [Char]
rgbToP3 (Rgb r g b) = unwords [show r, show g, show b]


flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

contentToP3 :: [Rgb] -> [[Char]]
contentToP3 = map rgbToP3

imgToP3Format:: Image -> [[Char]]
imgToP3Format (Image w h content) = [p3] ++[unwords [show w,show h]] ++ [maxValueColor] ++ contentToP3 (flatten content)

saveImage :: FilePath -> Image -> IO()
saveImage filePath img = writeFile filePath (unlines (imgToP3Format img))

