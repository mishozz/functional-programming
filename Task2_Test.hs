import Task2 (grayscale,Rgb(Rgb),Image(Image))
import Test.HUnit

inputImage1x1 :: Image
inputImage1x1 = Image 1 1 [[Rgb 50 70 90]]
expectedOutputImage1x1 :: Image
expectedOutputImage1x1 = Image 1 1 [[Rgb 66 66 66]]

inputImage1x2 = Image 1 2 [[Rgb 78 22 3],[Rgb 90 23 33]]
expectedOutputImage1x2 = Image 1 2 [[Rgb 37 37 37],[Rgb 44 44 44]]

inputImage3x1 = Image 3 1 [[Rgb 255 0 0, Rgb 155 128 0, Rgb 255 255 0]]
expectedOutputImage3x1 = Image 3 1 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227]]

inputImage3x2 :: Image 
inputImage3x2 = Image 3 2 [[Rgb 255 0 0, Rgb 155 128 0, Rgb 255 255 0],[Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]
expectedOutputImage3x2 = Image 3 2 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227], [Rgb 150 150 150, Rgb 255 255 255, Rgb 203 203 203]]

emptyImage :: Image
emptyImage = Image 0 0 [[]]

test1 :: Test
test1 = TestCase $ do
    assertEqual "grascale empty image should return empty image" emptyImage (grayscale emptyImage)
    assertEqual "grayscale should return Image 1 2 [[]]" expectedOutputImage1x2 (grayscale inputImage1x2)
    assertEqual "grayscale should return Image 1 1 [[Rgb 66 66 66]]" expectedOutputImage1x1 (grayscale inputImage1x1)
    assertEqual "grayscale should return  Image 3 2 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227], [Rgb 150 150 150, Rgb 255 255 255, Rgb 203 203 203]]"
     expectedOutputImage3x2 (grayscale inputImage3x2)
    assertEqual "graysca;e should return Image 3 1 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227]]" expectedOutputImage3x1 (grayscale inputImage3x1)

tl :: Test
tl =  TestList [test1]


main = runTestTT tl

