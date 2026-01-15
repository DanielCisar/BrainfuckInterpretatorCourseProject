module BrainFuck.Synthesis.Search where

    import BrainFuck.Synthesis.GenerateLoops
    import BrainFuck.Synthesis.Run
    import BrainFuck.Synthesis.Types 
    import Data.List (foldl')
    import BrainFuck.Types
    
    findProgram :: Int -> Int -> [Example] -> Maybe Program
    findProgram maxLen maxSteps examples = help 0
        where 
            help n 
                | n > maxLen = Nothing
                | otherwise = 
                    let progs = programsWithLoops n 2
                        initialCandidates = map (\p -> initCandidate p examples) progs
                        in helper (runCandidates examples initialCandidates maxSteps)
                            where 
                                helper (Just prog) = Just prog
                                helper Nothing = help (n + 1)

    runCandidates :: [Example] -> [Candidate] -> Int -> Maybe Program
    runCandidates _ [] _ = Nothing
    runCandidates _ _ 0  = Nothing
    runCandidates examples candidates stepsLeft = help stepsLeft candidates
        where 
            help 0 _ = Nothing
            help k cs = 
                let results = map (stepCandidate examples) cs
                    succs = [p | Success p <- results]
                in helper succs results
                    where 
                        helper (p:_) _ = Just p 
                        helper [] results = 
                            let liveProceses = [x | Alive x <- results]
                            in if null liveProceses
                                then Nothing
                                else help (k - 1) liveProceses


    