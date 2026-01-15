module BrainFuck.Binary where
    import BrainFuck.Types
    import BrainFuck.Parser
    import qualified Data.ByteString as BS
    import Data.Word (Word8)

    byteToBits :: Word8 -> [Int]
    byteToBits n = 
        helper (fromIntegral n) []
        where
            helper n acc
                | n == 0 && length acc == 8 = acc
                | n == 0 = helper n (0 : acc)
                | otherwise = helper (n `div` 2) ((n `mod` 2) : acc)
        
    groupBits :: [Int] -> [[Int]]
    groupBits [] = []
    groupBits xs 
        | length xs < 3 = []
        | otherwise = [take 3 xs] ++ groupBits (drop 3 xs)

    parseBits :: [Int] -> Maybe Instruction
    parseBits [0,0,0] = Just MoveRight
    parseBits [0,0,1] = Just MoveLeft
    parseBits [0,1,0] = Just Inc
    parseBits [0,1,1] = Just Dec
    parseBits [1,0,0] = Just Write
    parseBits [1,0,1] = Just Read
    parseBits [1,1,0] = Just LoopBegin
    parseBits [1,1,1] = Just LoopEnd
    parseBits _ = Nothing

    parseBinary :: FilePath -> IO (Result Program)
    parseBinary path = do
        content <- BS.readFile path
        let allBits = concat $ map byteToBits (BS.unpack content)
        let splitByThree = groupBits allBits
        let instructionList = map parseBits splitByThree
        let prog = helper instructionList
        return $
            if checkBrackets prog
                then Right prog
                else Left UnmatchedBrackets
            where
                helper [] = []
                helper (Just ins : xs) = ins : helper xs
                helper (Nothing : xs) = helper xs
