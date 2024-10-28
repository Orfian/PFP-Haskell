data Triple a = Triple a a a deriving (Show, Eq)

-- Define the TernaryTree type which is either a branch of references or a leaf with values
data TernaryTree a = Branch (Triple (TernaryTree a)) | Leaf (Triple a)
    deriving (Show, Eq)

-- Create an array of given size with default values
createArray :: Int -> a -> TernaryTree a
createArray n defaultValue
    | n <= 3    = Leaf (Triple defaultValue defaultValue defaultValue) -- Small array as single leaf
    | otherwise = createTree n defaultValue

-- Helper function to create a balanced ternary tree
createTree :: Int -> a -> TernaryTree a
createTree n defaultValue =
    let childSize = (n + 2) `div` 3  -- Calculate size of each child
    in Branch (Triple (createArray childSize defaultValue)
                      (createArray childSize defaultValue)
                      (createArray childSize defaultValue))

-- Function to enumerate all values in the array
enumerate :: TernaryTree a -> [a]
enumerate (Leaf (Triple left middle right)) = [left, middle, right]
enumerate (Branch (Triple left middle right)) = enumerate left ++ enumerate middle ++ enumerate right

-- Get a value by index
getElementAt :: Int -> TernaryTree a -> a
getElementAt index (Leaf (Triple left middle right)) =
    case index of
        0 -> left
        1 -> middle
        2 -> right
        _ -> error "Index out of range for a triplet"
getElementAt index (Branch (Triple left middle right)) =
    let childSize = countNodes left  -- Assuming all children have the same number of nodes
    in case index `div` childSize of
        0 -> getElementAt index left
        1 -> getElementAt (index - childSize) middle
        2 -> getElementAt (index - 2 * childSize) right
        _ -> error "Index out of range in ternary tree"

-- Count the number of nodes in a tree to assist with indexing
countNodes :: TernaryTree a -> Int
countNodes (Leaf _) = 3
countNodes (Branch (Triple left middle right)) = countNodes left + countNodes middle + countNodes right

-- Set a value at a given index (returns a new tree with the updated value)
setElementAt :: Int -> a -> TernaryTree a -> TernaryTree a
setElementAt index newValue (Leaf (Triple left middle right)) =
    case index of
        0 -> Leaf (Triple newValue middle right)
        1 -> Leaf (Triple left newValue right)
        2 -> Leaf (Triple left middle newValue)
        _ -> error "Index out of range for a triplet"
setElementAt index newValue (Branch (Triple left middle right)) =
    let childSize = countNodes left
        (newLeft, newMiddle, newRight) = case index `div` childSize of
            0 -> (setElementAt index newValue left, middle, right)
            1 -> (left, setElementAt (index - childSize) newValue middle, right)
            2 -> (left, middle, setElementAt (index - 2 * childSize) newValue right)
            _ -> error "Index out of range in ternary tree"
    in Branch (Triple newLeft newMiddle newRight)

main :: IO ()
main = do
    -- Create an array of size 9 with a default value of 0
    let array = createArray 9 0
    print $ enumerate array  -- Prints the initial array values

    -- Get element at index 5
    print $ getElementAt 5 array

    -- Set a new value at index 5 and print the modified array
    let updatedArray = setElementAt 5 42 array
    print $ enumerate updatedArray

    -- Access the updated element to verify
    print $ getElementAt 5 updatedArray
