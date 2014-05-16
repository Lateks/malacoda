import Data.Maybe (fromMaybe)
import Data.Array.Diff (Array, DiffArray, listArray, (!), (//))
import Data.Char (ord, isAscii, isSpace, chr)
import System.IO (putChar, putStrLn, withFile, hGetContents, Handle, IOMode(..), isEOF)
import System.Environment (getArgs)
import Control.Monad.Trans.State.Lazy (StateT, runStateT, get, put)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)

data MachineState = MachineState {
    a    :: Int,
    c    :: Int,
    d    :: Int,
    mem  :: Memory
}

type Memory = DiffArray Int Int

setRegA v state = state { a = v }
setRegC v state = state { c = v }
setRegD v state = state { d = v }
setMem  m state = state { mem = m }
wrapRegC state = state { c = wrapNum $ c state }
wrapRegD state = state { d = wrapNum $ d state }
mangleMemory state = let mem' = mem state
                         c'   = c state
                         in state { mem = mem' // [(c', mangleInstruction $ mem' ! c')] }

numericMax :: Int
numericMax = 59048

main :: IO ()
main = do
        args <- getArgs
        if length args < 1
            then putStrLn $ errorMsg "no input files"
            else do
                memory <- withFile (head args) ReadMode initializeMemory
                run $ listArray (0, numericMax) memory
                putChar '\n'

initializeMemory :: Handle -> IO [Int]
initializeMemory h = do
        source <- hGetContents h
        let instrs = filter (not . isSpace) source
        if length instrs > numericMax + 1
            then error $ errorMsg "input file too long"
            else readProgramToMemory instrs

run :: Memory -> IO ((), MachineState)
run memory = runStateT eval MachineState { a = 0, c = 0, d = 0, mem = memory }

eval :: StateT MachineState IO ()
eval = do
        state <- get
        let instr = getInstruction (c state) $ chr $ memory ! c state
            datav = memory ! d state
            memory = mem state
            updateMem = updateAccAndDataPtr state

        if validInstruction instr
            then case instr of
                'j' -> recurWith $ setRegD datav state
                'i' -> recurWith $ setRegC datav state
                '*' -> recurWith . updateMem $ rotateTernary datav
                'p' -> recurWith . updateMem $ op (a state) datav
                '<' -> do putAscii $ a state
                          recurWith state
                '/' -> do c <- getAscii
                          recurWith $ setRegA c state
                'v' -> return ()
                _   -> recurWith state
            else eval

    where recurWith state = do put . wrapRegD . wrapRegC . mangleMemory $ state
                               eval

          updateAccAndDataPtr state val = let updatedMem = mem state // [(d state, val)]
                                              in setRegA val $ setMem updatedMem state

          putAscii ic = let asciic = ic `mod` 128 -- emulate putc
                            ch = if asciic == 10 then '\n' else chr asciic
                            in liftIO $ putChar ch

          getAscii = liftIO $
            do eof <- isEOF
               if eof
                   then return numericMax
                   else do c <- getChar
                           return $ if c == '\n' then 10 else ord c

readProgramToMemory :: String -> IO [Int]
readProgramToMemory program =
    do let programMemory = map extract $ mapWithIndex readInstructionToMemory program
           n = length programMemory
       if n < 2
           then error $ errorMsg "invalid program (too short)"
           else return $ fillMemory programMemory n

     where mapWithIndex f = zipWith f [0..]

           extract = fromMaybe (error $ errorMsg "invalid instruction in source file")

fillMemory :: [Int] -> Int -> [Int]
fillMemory memory len = let toPair (x:y:xs) = (x, y)
                            uninitMem = numericMax + 1 - len
                            (a, dp) = toPair $ drop (len-2) memory
                            in memory ++ populateMem uninitMem a dp

populateMem :: Int -> Int -> Int -> [Int]
populateMem 0 _ _ = []
populateMem n a b = v : populateMem (n-1) b v
    where v = op b a

readInstructionToMemory :: Int -> Char -> Maybe Int
readInstructionToMemory creg c = do
    guard $ isAscii c
    if validInstruction c
        then do guard $ getInstruction creg c `elem` definedInstructions
                return $ ord c
        else return $ ord c
    where definedInstructions = "ji*p</vo"

getInstruction :: Int -> Char -> Char
getInstruction creg c = instructions !! ((ord c - 33 + creg) `mod` 94)
    where instructions = "+b(29e*j1VMEKLyC})8&m#~W>qxdRp0wkrUo[D7,XTcA\"lI.v%{gJh4G\\-=O@5`_3i<?Z';FNQuY]szf$!BS/|t:Pn6^Ha"

validInstruction :: Char -> Bool
validInstruction c = cval > 32 && cval < 127
    where cval = ord c

mangleInstruction :: Int -> Int
mangleInstruction instr = ord $ replacement !! (instr - 33)
    where replacement = "5z]&gqtyfr$(we4{WP)H-Zn,[%\\3dL+Q;>U!pJS72FhOA1CB6v^=I_0/8|jsb9m<.TVac`uY*MK'X~xDl}REokN:#?G\"i@"

rotateTernary :: Int -> Int
rotateTernary val = qt + rm * 19683
    where (qt, rm) = divMod val 3

op :: Int -> Int -> Int
op a dp = sum $ map f ternaryPowers
    where
        f :: Int -> Int
        f p = p * trits ! (index dp p, index a p)

        index :: Int -> Int -> Int
        index x p = (x `quot` p) `mod` 9

        ternaryPowers = take 5 [3^p | p <- [0,2..]]

        trits :: Array (Int, Int) Int
        trits = listArray ((0,0), (8,8))
                          [4, 3, 3, 1, 0, 0, 1, 0, 0,
                           4, 3, 5, 1, 0, 2, 1, 0, 2,
                           5, 5, 4, 2, 2, 1, 2, 2, 1,
                           4, 3, 3, 1, 0, 0, 7, 6, 6,
                           4, 3, 5, 1, 0, 2, 7, 6, 8,
                           5, 5, 4, 2, 2, 1, 8, 8, 7,
                           7, 6, 6, 7, 6, 6, 4, 3, 3,
                           7, 6, 8, 7, 6, 8, 4, 3, 5,
                           8, 8, 7, 8, 8, 7, 5, 5, 4]

errorMsg :: String -> String
errorMsg msg = "malbolge: " ++ msg

wrapNum :: Int -> Int
wrapNum n = (n + 1) `mod` numericMax
