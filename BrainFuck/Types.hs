module BrainFuck.Types where

    data Instruction = 
        MoveRight
        | MoveLeft 
        | Inc 
        | Dec
        | Write
        | Read 
        | LoopBegin
        | LoopEnd
        deriving (Eq, Show)

    type Program = [Instruction]

    type Cell = Int 

    type Input = [Cell]

    type Output = [Cell]

    data Tape = Tape 
        {
            left :: [Cell],
            curr :: Cell,
            right :: [Cell]
        }
        deriving (Eq, Show)


    type IP = Int 

    data State = State 
        {
            program :: Program,
            ip :: IP,
            tape :: Tape,
            input :: Input,
            output :: Output
        }
        deriving (Eq, Show)

    data BFError = 
            UnmatchedBrackets
            | InputExhausted
        deriving (Eq, Show) 

    type Result a = Either BFError a 



