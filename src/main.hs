-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage x = print x
-- Write division here
division :: Double -> Double -> Maybe Double
division _ 0 = Nothing
division x y = Just (x / y)
-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)
-- Write factList here
factList :: Int -> [Int]
factList n = [factorial i | i <- [1..n]]
-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

main :: IO ()
main = do
    -- Testing printAMessage
    putStrLn "\nTesting printAMessage:"
    printAMessage "Hello World!"

    -- Testing division
    putStrLn "\nTesting division:"
    let z = division 1 2
    let w = division 1 0
    let g = division 6 2
    print ("z: " ++ show z ++ ", w: " ++ show w ++ ", g: " ++ show g)

    -- Testing factorial
    putStrLn "\nTesting factorial:"
    let a = factorial 1
    let b = factorial 7
    print ("a: " ++ show a ++ ", b: " ++ show b)

    -- Testing factList
    putStrLn "\nTesting factList:"
    let testList = factList 5
    print ("testList: " ++ show testList)

    -- Testing merge
    putStrLn "\nTesting merge:"
    let merged = merge [1, 3, 6] [2, 4, 5, 6, 7]
    print ("merged: " ++ show merged)