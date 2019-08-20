module SimpleAssembler
    (
        --simpleAssembler,
        updateRegister
    ) where
import qualified Data.Map.Strict as M

-- We want to create a simple interpreter of assembler which will support the following instructions:

-- mov x y - copies y (either a constant value or the content of a register) into register x
-- inc x - increases the content of register x by one
-- dec x - decreases the content of register x by one
-- jnz x y - jumps to an instruction y steps away (positive means forward, negative means backward), but only if x (a constant or a register) is not zero

-- Register names are alphabetical (letters only). Constants are always integers (positive or negative).
-- The function will take an input list with the sequence of the program instructions 
-- and will return a dictionary with the contents of the registers.

-- ex.
-- simpleAssembler ["mov a 5", "inc a", "dec a", "dec a", "jnz a -1", "inc a"]

-- ''' visualized:
-- mov a 5
-- inc a
-- dec a
-- dec a
-- jnz a -1
-- inc a
-- '''
-- return fromList [("a", 1)]
type Registers = M.Map String Int
data Register a b = Uninitialized | Initialized a b deriving (Show)
type Cmds a = M.Map String a

-- type Register = (String, Int)
-- data Register a b = Uninitialized | Initialized a b deriving (Show)

-- reduce the set of instructions to a list of Registers containing their final values
simpleAssembler :: [String] -> Registers
simpleAssembler cmdStrings = 
    -- convert cmdStrings to commands
-- needs list of cmds and registers accumulator
executeCmd :: [String] -> 
    
cmds :: Cmds
cmds :: Map.fromList
[("mov", mov)
,("inc",(Free, "JAH3I"))
,("dec",(Free, "IQSA9"))
,("jnz",(Free, "QOTSA"))
]
updateRegister :: Int -> Register -> Register 
updateRegister y (name, x) = (name, y)

mov :: Int -> String -> Registers -> Registers
mov y name registers = 

stringToCmd :: String 