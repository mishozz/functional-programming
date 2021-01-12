import Task1 (values,Tree(EmptyTree,Node),Strategy(Inorder,Postorder,Preorder))
import Test.HUnit

testTreeHasOnlyRoot = Node 5 EmptyTree EmptyTree 
testTree2 = Node 5 (Node 22 EmptyTree EmptyTree) (Node 1 EmptyTree EmptyTree)
testTree3 = Node 5 (Node 22 (Node 2 EmptyTree EmptyTree) (Node 6 EmptyTree  EmptyTree)) (Node 1 EmptyTree (Node 3 (Node 111 EmptyTree  EmptyTree ) EmptyTree))
testTree4 = Node 1 (Node 2 (Node 4 EmptyTree  EmptyTree ) (Node 5 EmptyTree  EmptyTree )) (Node 3 EmptyTree  EmptyTree )

test1 :: Test
test1 = TestCase $ do
    assertEqual  "values with Inorder strategy and EmptyTree should return empty list" ([] :: [()]) (values Inorder EmptyTree)
    assertEqual  "values with Inorder strategy and tree = (Node 5 EmptyTree EmptyTree) should return [5]" ([5] :: [Int]) (values Inorder testTreeHasOnlyRoot)
    assertEqual  "values with Inorder strategy and tree = Node 5 (Node 22 EmptyTree EmptyTree) (Node 1 EmptyTree EmptyTree) should return [22,5,1]"
     ([22,5,1] :: [Int]) (values Inorder testTree2)
    assertEqual  "values with Inorder strategy and tree = Node 5 (Node 22 (Node 2 EmptyTree EmptyTree) (Node 6 EmptyTree  EmptyTree)) (Node 1 EmptyTree (Node 3 (Node 111 EmptyTree  EmptyTree ) EmptyTree)) should return [2, 22, 6, 5, 1, 111, 3]"
     ([2, 22, 6, 5, 1, 111, 3] :: [Int]) (values Inorder  testTree3)
    assertEqual  "values with Inorder strategy and tree = Node 1 (Node 2 (Node 4 EmptyTree  EmptyTree ) (Node 5 EmptyTree  EmptyTree )) (Node 3 EmptyTree  EmptyTree ) should return [4,2,5,1,3]"
     ([4,2,5,1,3]::[Int]) (values Inorder testTree4) 

test2 :: Test
test2 = TestCase $ do
    assertEqual  "values with Postorder strategy and EmptyTree should return empty list" ([] :: [()]) (values Postorder EmptyTree)
    assertEqual  "values with Postorder strategy and tree = (Node 5 EmptyTree EmptyTree) should return [5]" ([5] :: [Int]) (values Postorder testTreeHasOnlyRoot)
    assertEqual  "values with Postorder strategy and tree = Node 5 (Node 22 EmptyTree EmptyTree) (Node 1 EmptyTree EmptyTree) should return [1,5,22]"
     ([22,1,5] :: [Int]) (values Postorder testTree2)
    assertEqual  "values with Postorder strategy and tree = Node 5 (Node 22 (Node 2 EmptyTree EmptyTree) (Node 6 EmptyTree  EmptyTree)) (Node 1 EmptyTree (Node 3 (Node 111 EmptyTree  EmptyTree ) EmptyTree)) should return [2,6,22,111,3,1,5]"
     ([2,6,22,111,3,1,5] :: [Int]) (values Postorder testTree3)
    assertEqual  "values with Postorder strategy and tree = Node 1 (Node 2 (Node 4 EmptyTree  EmptyTree ) (Node 5 EmptyTree  EmptyTree )) (Node 3 EmptyTree  EmptyTree ) should return [4,5,2,3,1]"
     ([4,5,2,3,1]::[Int]) (values Postorder testTree4) 


test3 :: Test
test3 = TestCase $ do
    assertEqual  "values with Preorder stratey and EmptyTree should return empty list" ([] :: [()]) (values Inorder EmptyTree)
    assertEqual  "values with Preorder strategy and tree = (Node 5 EmptyTree EmptyTree) should return [5]" ([5] :: [Int]) (values Preorder  testTreeHasOnlyRoot)
    assertEqual  "values with Preorder strategy and tree = Node 5 (Node 22 (Node 2 EmptyTree EmptyTree) (Node 6 EmptyTree  EmptyTree)) (Node 1 EmptyTree (Node 3 (Node 111 EmptyTree  EmptyTree ) EmptyTree)) should return [5,22,2,6,1,3,111]"
     ([5,22,2,6,1,3,111] :: [Int]) (values Preorder  testTree3)
    assertEqual  "values with Preorder strategy and tree = Node 1 (Node 2 (Node 4 EmptyTree  EmptyTree ) (Node 5 EmptyTree  EmptyTree )) (Node 3 EmptyTree  EmptyTree ) should return [1,2,4,5,3]"
     ([1,2,4,5,3]::[Int]) (values Preorder  testTree4) 


tl :: Test
tl = TestList [test1,test2,test3]

main :: IO Counts
main = runTestTT tl