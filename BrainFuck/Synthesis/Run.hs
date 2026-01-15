module BrainFuck.Synthesis.Run where

    import BrainFuck.Types
    import BrainFuck.Interpreter
    import BrainFuck.Tape
    import BrainFuck.Synthesis.Types

    initCandidate :: Program -> [Example] -> Candidate
    initCandidate prog examples = 
        Candidate
            {
                candProgram = prog, 
                candStates = map makeStates examples
            }
            where 
                makeStates (i, _) = 
                    State
                    {
                        program = prog,
                        ip = 0,
                        tape = emptyTape,
                        input = i,
                        output = []
                    }

    finished :: State -> Bool 
    finished s = 
        ip s < 0 || ip s >= length (program s)

    stepAll :: [State] -> Result [State]
    stepAll [] = Right []
    stepAll (curr : states) = 
        helper (step curr)
            where 
                helper (Left err) = Left err 
                helper (Right res) = 
                    deepHelper (stepAll states)
                        where 
                            deepHelper (Left err) = Left err
                            deepHelper (Right res') = Right (res : res')

    checkOutputs :: [Example] -> [State] -> Program -> StepResult
    checkOutputs examples states prog = 
        if (helper examples states) then Success prog else Dead
            where 
                helper [] _ = True
                helper ((_, exp):examp) (s:st)
                    | output s == exp = helper examp st
                    | otherwise = False

    stepCandidate :: [Example] -> Candidate -> StepResult
    stepCandidate examples cand = 
        helper (stepAll (candStates cand))
            where
                helper (Left err) = Dead
                helper (Right newStates) = 
                    if and $ map finished newStates
                        then checkOutputs examples newStates (candProgram cand)
                        else Alive cand {candStates = newStates}
