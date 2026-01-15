module BrainFuck.Synthesis.Types where

    import BrainFuck.Types
    import BrainFuck.Interpreter

    type Example = (Input, Output)

    data Candidate = Candidate
        {
            candProgram :: Program, 
            candStates :: [State]
        }

    data StepResult
        = Alive Candidate
        | Dead
        | Success Program
