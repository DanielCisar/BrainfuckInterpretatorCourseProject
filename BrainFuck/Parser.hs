    module BrainFuck.Parser where

        import BrainFuck.Types
        import BrainFuck.Tape
        import Data.Maybe (mapMaybe)

        charToInstructions :: Char -> Maybe Instruction
        charToInstructions '>' = Just MoveRight 
        charToInstructions '<' = Just MoveLeft
        charToInstructions '+' = Just Inc
        charToInstructions '-' = Just Dec
        charToInstructions '.' = Just Write
        charToInstructions ',' = Just Read
        charToInstructions '[' = Just LoopBegin
        charToInstructions ']' = Just LoopEnd
        charToInstructions _ = Nothing 


       

        checkBrackets :: Program -> Bool
        checkBrackets pr = 
            helper pr 0
                where
                    helper [] depth = depth == 0
                    helper (i:prog) depth
                        | depth < 0 = False
                        | i == LoopBegin = helper prog (depth + 1)
                        | i == LoopEnd = helper prog (depth - 1)
                        | otherwise = helper prog depth 


        parseProgram :: String -> Result Program
        parseProgram str = 
            let pr = mapMaybe charToInstructions str 
            in if checkBrackets pr then Right pr else Left UnmatchedBrackets