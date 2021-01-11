data Tree a = EmptyTree | Node {
                            value :: a,
                            left :: Tree a,
                            right :: Tree a
                         } deriving (Show,Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show,Read)

preorderTraversal :: Tree a -> [a]
preorderTraversal EmptyTree = []
preorderTraversal (Node a left right) = a : preorderTraversal left ++ preorderTraversal right

inorderTraversal :: Tree a -> [a]
inorderTraversal EmptyTree = []
inorderTraversal (Node a left right) = inorderTraversal left ++ [a] ++ inorderTraversal right

postorderTraversal :: Tree a -> [a]
postorderTraversal EmptyTree = []
postorderTraversal (Node a left right) =  postorderTraversal left ++ postorderTraversal right ++ [a]

values :: Strategy -> Tree a -> [a]
values Inorder  = inorderTraversal 
values Postorder = postorderTraversal
values Preorder = preorderTraversal