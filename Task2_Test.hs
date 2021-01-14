import Task2 (grayscale,imgToP3Format,floodFill,toCorrectContent,transfromToWord8,Rgb(Rgb),Image(Image))
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

rgb10x10 :: [[Rgb]]
rgb10x10 = [[Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 100 100 100, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 100 100 100,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 100 100 100,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 100 100 100,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 100 100 100,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 100 100 100,Rgb 100 100 100, Rgb 100 100 100, Rgb 100 100 100,Rgb 100 100 100,Rgb 100 100 100],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128]]

expectedRgb10x10 :: [[Rgb]]
expectedRgb10x10 = [[Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0,Rgb 100 100 100, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128,Rgb 128 128 128],
                        [Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 100 100 100, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128],
                        [Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 100 100 100, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128],
                        [Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 100 100 100, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128],
                        [Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 100 100 100, Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128, Rgb 128 128 128, Rgb 128 128 128],
                        [Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 100 100 100,Rgb 100 100 100, Rgb 100 100 100,Rgb 100 100 100, Rgb 100 100 100, Rgb 100 100 100],
                        [Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0],
                        [Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0],
                        [Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0],
                        [Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0, Rgb 255 0 0,Rgb 255 0 0]]

emptyImage :: Image
emptyImage = Image 0 0 [[]]

testGrayscaleSuccessful :: Test
testGrayscaleSuccessful = TestCase $ do
    assertEqual "grascale empty image should return empty image" emptyImage (grayscale emptyImage)
    assertEqual "grayscale should return Image 1 2 [[]]" expectedGrayscaleImage1x2 (grayscale inputImage1x2)
    assertEqual "grayscale should return Image 1 1 [[Rgb 66 66 66]]" expectedGrayscaleImage1x1 (grayscale inputImage1x1)
    assertEqual "grayscale should return  Image 3 2 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227], [Rgb 150 150 150, Rgb 255 255 255, Rgb 203 203 203]]"
     expectedGrayscaleImage3x2 (grayscale inputImage3x2)
    assertEqual "grayscale should return Image 3 1 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227]]" expectedGrayscaleImage3x1 (grayscale inputImage3x1)

testFloodFillReturnsNewImage :: Test
testFloodFillReturnsNewImage = TestCase $ do
    assertEqual "floodFill should return Image 3 3 [[Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0],[Rgb 255 255 255, Rgb 255 0 0, Rgb 255 255 255],[Rgb 255 0 0,Rgb 255 0 0,Rgb 255 0 0]]"
     (Image 3 3 expectedFloodFill3x3) (floodFill (Rgb 255 0 0)  0 0 (Image 3 3 rgb3x3))
    assertEqual "floodFill should return Image 3 2 [[Rgb 128 128 128, Rgb 128 128 128,Rgb 128 128 128],[Rgb 255 255 255, Rgb 255 255 255, Rgb 255 255 255]]"
     (Image 3 2 expectedFloodFill3x2) (floodFill (Rgb 128 128 128) 0 1  (Image 3 2 rgb3x2))
    assertEqual "floodFill should fill the whole Content"  (Image 9 9 expectedFloodFill9x9) (floodFill (Rgb 128 128 128) 4 3 (Image 9 9 rgb9x9))
    assertEqual "floodFill succesll Image 10 by 10" (Image 10 10 expectedRgb10x10) (floodFill (Rgb 255 0 0) 1 3 (Image 10 10 rgb10x10))

testFloodFillReturnSameImage :: Test
testFloodFillReturnSameImage = TestCase $ do
    assertEqual "floodFill with no different color should return the same Image" (Image 9 9 rgb9x9) (floodFill (Rgb 0 0 0) 4 3 (Image 9 9 rgb9x9))
    assertEqual "floodFill with no different color should return the same Image" (Image 9 9 rgb3x3) (floodFill (Rgb 255 255 255) 1 0 (Image 9 9 rgb3x3))

testFloodFillFail :: Test
testFloodFillFail = TestCase $ do  
    assertEqual  "floodFill should return error with out of bound cordinates" (error "Invalid cordinates x and y") (floodFill (Rgb 128 128 128) 10 10 (Image 9 9 rgb9x9))
    assertEqual  "floodFill should return error with negative cordinates" (error "Invalid cordinates x and y") (floodFill (Rgb 128 128 128) (-1) 10 (Image 9 9 rgb9x9))
    assertEqual  "floodFill should return error with Empty image" (error "Unable to floodFill empty Image content") (floodFill (Rgb 255 0 0) 0 0 emptyImage)

testImgToP3Format :: Test
testImgToP3Format = TestCase $ do
    assertEqual "imgToP3 should return list in the form of [<format>,<w>,<h>,<max color>, <rgb>]" ["P3","3 2","255","255 0 0","155 128 0","255 255 0","0 255 0","255 255 255","128 255 128"]
     (imgToP3Format inputImage3x2)
    assertEqual "imgToP3 should return list in the form of [<format>,<w>,<h>,<max color>, <rgb>]" ["P3","3 1","255","255 0 0","155 128 0","255 255 0"]
     (imgToP3Format inputImage3x1)

testCorrectContent :: Test 
testCorrectContent = TestCase $ do
    assertEqual "toCorrectContent should return Image content 3 by 2"
     [[Rgb 76 76 76, Rgb 76 76 76, Rgb 76 76 76],[Rgb 76 76 76, Rgb 76 76 76, Rgb 76 76 76]]
      (toCorrectContent [[Rgb 76 76 76],[Rgb 76 76 76],[Rgb 76 76 76],[Rgb 76 76 76],[Rgb 76 76 76],[Rgb 76 76 76]] 3)
    assertEqual "toCorrectContent should return Image content 1 by 1"  [[Rgb 76 76 76]] (toCorrectContent [[Rgb 76 76 76]] 1)
    assertEqual "toCorrectContent should return Image content 3 by 3"
     [[Rgb 255 0 0, Rgb 255 0 0, Rgb 255 0 0],[Rgb 255 255 255, Rgb 255 0 0, Rgb 255 255 255],[Rgb 255 0 0,Rgb 255 0 0,Rgb 255 0 0]]
     (toCorrectContent [[Rgb 255 0 0], [Rgb 255 0 0], [Rgb 255 0 0],[Rgb 255 255 255], [Rgb 255 0 0], [Rgb 255 255 255],[Rgb 255 0 0],[Rgb 255 0 0,Rgb 255 0 0]] 3)
    assertEqual "tocorrectContent should return empty content" [[]] (toCorrectContent [[]] 0)

testTransfromToWord8 :: Test
testTransfromToWord8 = TestCase $ do 
    assertEqual "transfromToWord8 should return 255" 255 (transfromToWord8 (Rgb 255 255 255))
    assertEqual "transfromToWord8 should return 128" 128 (transfromToWord8 (Rgb 128 128 128))
    assertEqual "transfromToWord8 should return 0" 0 (transfromToWord8 (Rgb 0 0 0))
    assertEqual "transfromToWord8 should return 227" 227 (transfromToWord8 (Rgb 255 255 0))
    assertEqual "transfromToWord8 should return 227" 76 (transfromToWord8 (Rgb 255 0 0))



tl :: Test
tl =  TestList [testGrayscaleSuccessful, testFloodFillReturnsNewImage
    ,testFloodFillReturnSameImage,testImgToP3Format,testCorrectContent,
    testTransfromToWord8]


main = runTestTT tl
