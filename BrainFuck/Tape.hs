module BrainFuck.Tape where
    import BrainFuck.Types

    emptyTape :: Tape 
    emptyTape = Tape [] 0 [] 

    moveLeft :: Tape -> Tape 
    moveLeft t@(Tape (x:xs) c ys) = Tape xs x (c:ys)
    moveLeft (Tape [] c ys) = Tape [] 0 (c:ys)

    moveRight :: Tape -> Tape 
    moveRight t@(Tape xs c (y:ys)) = Tape (c:xs) y ys 
    moveRight (Tape xs c []) = Tape (c:xs) 0 []

    readCell :: Tape -> Cell
    readCell t@(Tape _ c _) = c 

    writeCell :: Cell -> Tape -> Tape 
    writeCell v (Tape xs _ ys) = Tape xs v ys 