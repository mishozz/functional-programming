import Task2 (grayscale,floodFill,Rgb(Rgb),Image(Image))
import Test.HUnit

inputImage1x1 :: Image
inputImage1x1 = Image 1 1 [[Rgb 50 70 90]]
expectedGrayscaleImage1x1 :: Image
expectedGrayscaleImage1x1 = Image 1 1 [[Rgb 66 66 66]]

inputImage1x2 = Image 1 2 [[Rgb 78 22 3],[Rgb 90 23 33]]
expectedGrayscaleImage1x2 = Image 1 2 [[Rgb 37 37 37],[Rgb 44 44 44]]

inputImage3x1 = Image 3 1 [[Rgb 255 0 0, Rgb 155 128 0, Rgb 255 255 0]]
expectedGrayscaleImage3x1 = Image 3 1 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227]]

inputImage3x2 :: Image 
inputImage3x2 = Image 3 2 [[Rgb 255 0 0, Rgb 155 128 0, Rgb 255 255 0],[Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]
expectedGrayscaleImage3x2 = Image 3 2 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227], [Rgb 150 150 150, Rgb 255 255 255, Rgb 203 203 203]]

rgb3x3 :: [[Rgb]]
rgb3x3 = [[Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],[Rgb 255 255 255, Rgb 0 0 0, Rgb 255 255 255],[Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0]]

rgb3x2 :: [[Rgb]]
rgb3x2 = [[Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],[Rgb 255 255 255, Rgb 255 255 255, Rgb 255 255 255]]

expectedFloodFill3x3 :: [[Rgb]]
expectedFloodFill3x3 = [[Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0],[Rgb 255 255 255, Rgb 255 0 0, Rgb 255 255 255],[Rgb 255 0 0,Rgb 255 0 0,Rgb 255 0 0]]

expectedFloodFill3x2 :: [[Rgb]]
expectedFloodFill3x2 = [[Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],[Rgb 255 255 255, Rgb 255 255 255, Rgb 255 255 255]]

rgb9x9 :: [[Rgb]]
rgb9x9 = [[Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],
            [Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],
            [Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],
            [Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],
            [Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],
            [Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],
            [Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],
            [Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0],
            [Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0,Rgb 0 0 0]]

expectedFloodFill9x9 :: [[Rgb]]
expectedFloodFill9x9 = [[Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128]]

emptyImage :: Image
emptyImage = Image 0 0 [[]]

testGrayscaleSuccessful :: Test
testGrayscaleSuccessful = TestCase $ do
    assertEqual "grascale empty image should return empty image" emptyImage (grayscale emptyImage)
    assertEqual "grayscale should return Image 1 2 [[]]" expectedGrayscaleImage1x2 (grayscale inputImage1x2)
    assertEqual "grayscale should return Image 1 1 [[Rgb 66 66 66]]" expectedGrayscaleImage1x1 (grayscale inputImage1x1)
    assertEqual "grayscale should return  Image 3 2 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227], [Rgb 150 150 150, Rgb 255 255 255, Rgb 203 203 203]]"
     expectedGrayscaleImage3x2 (grayscale inputImage3x2)
    assertEqual "graysca;e should return Image 3 1 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227]]" expectedGrayscaleImage3x1 (grayscale inputImage3x1)

testFloodFillReturnsNewImage :: Test
testFloodFillReturnsNewImage = TestCase $ do
    assertEqual "floodFill should return Image 3 3 [[Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0],[Rgb 255 255 255, Rgb 255 0 0, Rgb 255 255 255],[Rgb 255 0 0,Rgb 255 0 0,Rgb 255 0 0]]"
     (Image 3 3 expectedFloodFill3x3) (floodFill (Rgb 255 0 0)  0 0 (Image 3 3 rgb3x3))
    assertEqual "floodFill should return Image 3 2 [[Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],[Rgb 255 255 255, Rgb 255 255 255, Rgb 255 255 255]]"
     (Image 3 2 expectedFloodFill3x2) (floodFill (Rgb 128 128 128) 0 1  (Image 3 2 rgb3x2))
    assertEqual "floodFill should fill the whole Content"  (Image 9 9 expectedFloodFill9x9) (floodFill (Rgb 128 128 128) 4 3 (Image 9 9 rgb9x9))

testFloodFillReturnSameImage :: Test
testFloodFillReturnSameImage = TestCase $ do
    assertEqual "floodFill with no different color should return the same Image" (Image 9 9 rgb9x9) (floodFill (Rgb 0 0 0) 4 3 (Image 9 9 rgb9x9))
    assertEqual "floodFill with no different color should return the same Image" (Image 9 9 rgb3x3) (floodFill (Rgb 255 255 255) 1 0 (Image 9 9 rgb3x3))

testFloodFillFail :: Test
testFloodFillFail = TestCase $ do  
    assertEqual  "floodFill should return error with out of bound cordinates" (error "Invalid cordinates x and y") (floodFill (Rgb 128 128 128) 10 10 (Image 9 9 rgb9x9))
    assertEqual  "floodFill should return error with negative cordinates" (error "Invalid cordinates x and y") (floodFill (Rgb 128 128 128) (-1) 10 (Image 9 9 rgb9x9))
    assertEqual  "floodFill should return error with Empty image" (error "Unable to floodFill empty Image content") (floodFill (Rgb 255 0 0) 0 0 emptyImage)


tl :: Test
tl =  TestList [testGrayscaleSuccessful, testFloodFillReturnsNewImage,testFloodFillReturnSameImage]


main = runTestTT tl
