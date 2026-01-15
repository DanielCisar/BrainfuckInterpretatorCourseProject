module BrainFuck.Operators where

    import BrainFuck.Types
    import BrainFuck.Interpreter
    import BrainFuck.Parser

    concatenate :: Program -> Program -> Input -> Result Output
    concatenate p q startInput = 
        helper (execute p startInput)
            where 
                helper (Left err) = Left err 
                helper (Right res) = execute q res

    parallel :: Program -> Program -> Input -> Result Output
    parallel p q startInput = 
        helper (execute p startInput) (execute q startInput)
            where 
                helper (Left err) _ = Left err
                helper _ (Left err) = Left err 
                helper (Right res1) (Right res2) = 
                    Right (merge res1 res2)
                        where 
                            merge [] ys = ys
                            merge xs [] = xs
                            merge (x:xs) (y:ys) = x : y : merge xs ys
                                
    alternate :: Program -> Program -> Input -> Result Output
    alternate p q startInput = 
        let input1 = getOdd startInput 1
            input2 = getEven startInput 1
        in helper (execute p input1) (execute q input2)
            where 
                helper (Left err) _ = Left err
                helper _ (Left err) = Left err 
                helper (Right res1) (Right res2) = 
                    Right (merge res1 res2)
                merge [] ys = ys
                merge xs [] = xs
                merge (x:xs) (y:ys) = x : y : merge xs ys
                getOdd [] _ = []
                getOdd (x:xs) i
                    | i `mod` 2 == 1 = x : getOdd xs (i + 1)
                    | otherwise = getOdd xs (i + 1)
                getEven [] _ = []
                getEven (x:xs) i 
                    | i `mod` 2 == 0 = x : getEven xs (i + 1)
                    | otherwise = getEven xs (i + 1)

    runCombined :: (Program -> Program -> Input -> Result Output) ->
        FilePath -> FilePath -> Input -> IO (Result Output)
    runCombined op path1 path2 inp = do
        contents1 <- readFile path1 
        contents2 <- readFile path2 
        let prog1 = parseProgram contents1
        let prog2 = parseProgram contents2
        return $ do 
            p1 <- prog1
            p2 <- prog2
            op p1 p2 inp

    