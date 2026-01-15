module BrainFuck.Interpreter where
    import BrainFuck.Types
    import BrainFuck.Tape
    import BrainFuck.Parser


    step :: State -> Result State
    step s@(State prog pointer tp inp out)
        | pointer >= length prog = Right s 
        | otherwise = makeStep (prog !! pointer)
            where 
                makeStep MoveRight = Right s {tape = moveRight tp, ip = pointer + 1}
                makeStep MoveLeft = Right s {tape = moveLeft tp, ip = pointer + 1}
                makeStep Inc = Right s {tape = writeCell (readCell tp + 1) tp, ip = pointer + 1}
                makeStep Dec = Right s {tape = writeCell (readCell tp - 1) tp, ip = pointer + 1}
                makeStep Write = Right s {output = out ++ [readCell tp], ip = pointer + 1}
                makeStep Read = 
                    helper inp
                        where 
                            helper [] = Left InputExhausted
                            helper (x:xs) = Right s {tape = writeCell x tp, input = xs, ip = pointer + 1}
                makeStep LoopBegin = 
                    if readCell tp == 0 
                        then Right s {ip = findMatchingEnd prog pointer}
                        else Right s {ip = pointer + 1}
                makeStep LoopEnd = 
                    if readCell tp /= 0
                        then Right s {ip = findMatchingStart prog pointer}
                        else Right s {ip = pointer + 1}
                
    findMatchingEnd :: Program -> IP -> IP
    findMatchingEnd pr startIP = 
        helper (startIP + 1) 1
            where 
                helper p depth
                    | depth == 0 = p + 1
                    | pr !! p == LoopBegin = helper (p + 1) (depth + 1)
                    | pr !! p == LoopEnd = helper (p + 1) (depth - 1)
                    | otherwise = helper (p + 1) depth
        

    findMatchingStart :: Program -> IP -> IP 
    findMatchingStart pr startIP = 
        helper (startIP - 1) 1
            where 
                helper p depth
                    | depth == 0 = p + 2
                    | pr !! p  == LoopBegin = helper (p - 1) (depth - 1)
                    | pr !! p == LoopEnd = helper (p - 1) (depth + 1)
                    | otherwise = helper (p - 1) depth

    run :: State -> Result State
    run s@(State prog pointer _ _ _)
        | pointer < 0 || pointer >= length prog = Right s 
        | otherwise = 
            helper (step s)
                where 
                    helper (Left err) = Left err 
                    helper (Right next) = (run next)     

    execute :: Program -> Input -> Result Output
    execute prog inputData = 
        let startState = State {
            program = prog,
            ip = 0,
            tape = emptyTape,
            input = inputData,
            output = []
        }
        in 
            helper (run startState)
                where 
                    helper (Left err) = Left err 
                    helper (Right res) = Right (output res)

    brainfuck :: FilePath -> Input -> IO (Result Output)
    brainfuck filePath input = do 
        content <- readFile filePath
        helper (parseProgram content)
            where 
                helper (Left err) = return $ Left err
                helper (Right res) = return $ execute res input

                   