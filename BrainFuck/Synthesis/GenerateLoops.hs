module BrainFuck.Synthesis.GenerateLoops where 
    import BrainFuck.Types

    instructions :: [Instruction]
    instructions = 
        [ MoveRight, MoveLeft, Inc, Dec, Write, Read ]
    -- Idea: By Chat Gpt 5
    --I am writing a brainfuck interpretation. 
    --I have a structure for all commands such as moveForward ... 
    --I need a function that generates all programs with length n. 
    --But I want to add loops to this generations. I have a loopPen and loopClose command.
    --I want the function to take n -> maxLenght and maxDepth of the loops(2 nested cycles).
    --How do I non deterministically choose how to generate commands. I am writting in haskell
    --This is a classic and very Haskell-ish problem 🙂
-- You want to enumerate all Brainfuck programs up to:

-- a maximum total length n

-- a maximum loop nesting depth d

-- including [ / ] (loopOpen / loopClose)

-- generation should be non-deterministic → lists or List monad

-- Below is a clean, idiomatic way to structure this.

-- 1. Represent commands

-- First, assume something like this:

-- data Cmd
--   = MoveForward
--   | MoveBackward
--   | Inc
--   | Dec
--   | Output
--   | Input
--   | LoopOpen
--   | LoopClose
--   deriving (Eq, Show)

-- 2. Key idea: generate well-formed programs

-- You cannot generate [ and ] freely.
-- You must ensure:

-- loops are balanced

-- nesting depth ≤ maxDepth

-- total length ≤ maxLength

-- This means generation must track state:

-- remaining length

-- current loop depth

-- how many open loops are currently unclosed

-- 3. Core generator signature

-- This is the crucial function:

-- gen :: Int      -- remaining length
--     -> Int      -- current loop depth
--     -> Int      -- max loop depth
--     -> Int      -- open loops
--     -> [[Cmd]]


-- remaining length → stop when 0

-- current loop depth → prevent exceeding max depth

-- open loops → prevent closing when none are open

-- 4. Base case
-- gen 0 _ _ 0 = [[]]   -- valid only if no open loops
-- gen 0 _ _ _ = []     -- invalid if loops are still open

-- 5. Recursive generation (non-deterministic)

-- This is where the List monad shines:

-- gen n depth maxDepth open =
--   concat
--     [ simpleCmds
--     , openLoopCmds
--     , closeLoopCmds
--     ]
--   where
--     next = gen (n - 1)

--     simpleCmds =
--       [ cmd : rest
--       | cmd <- [MoveForward, MoveBackward, Inc, Dec, Output, Input]
--       , rest <- next depth maxDepth open
--       ]

--     openLoopCmds =
--       [ LoopOpen : rest
--       | depth < maxDepth
--       , rest <- next (depth + 1) maxDepth (open + 1)
--       ]

--     closeLoopCmds =
--       [ LoopClose : rest
--       | open > 0
--       , rest <- next (depth - 1) maxDepth (open - 1)
--       ]


-- This is pure nondeterminism:
-- every possible legal choice is explored.

-- 6. Public API
-- generatePrograms :: Int -> Int -> [[Cmd]]
-- generatePrograms maxLength maxDepth =
--   gen maxLength 0 maxDepth 0

    programsWithLoops :: Int -> Int -> [Program]
    programsWithLoops n maxDepth = 
        gen n 0 []
            where 
                gen 0 depth acc
                    | depth == 0 = [reverse acc]
                    | otherwise = []
                gen k depth acc = 
                    baseIns ++ loopOpen ++ loopClosed
                        where 
                            baseIns = 
                                [x 
                                | ins <- instructions, 
                                x <- gen (k - 1) depth (ins:acc)]
                            loopOpen = 
                                [x 
                                | depth < maxDepth, 
                                x <- gen (k - 1) (depth + 1) (LoopBegin : acc)]
                            loopClosed = 
                                [x
                                | depth > 0, 
                                x <- gen (k - 1) (depth - 1) (LoopEnd : acc)]