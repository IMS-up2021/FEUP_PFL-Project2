-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Data.List
import System.IO

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type Stack = [Int]

type State = [(String, Int)]

createEmptyStack :: Stack
createEmptyStack = [] 

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map show stack)

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," [v ++ "=" ++ show n | (v, n) <- sort state]

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (Push n:code, stack, state) = run (code, fromInteger n:stack, state)
run (Add:code, n2:n1:stack, state) = run (code, n1 + n2:stack, state)
run (Mult:code, n2:n1:stack, state) = run (code, n1 * n2:stack, state)
run (Sub:code, n2:n1:stack, state) = run (code, n1 - n2:stack, state)
run (Tru:code, stack, state) = run (code, 1:stack, state)
run (Fals:code, stack, state) = run (code, 0:stack, state)
run (Equ:code, n2:n1:stack, state) = run (code, if n1 == n2 then 1:stack else 0:stack, state)
run (Le:code, n2:n1:stack, state) = run (code, if n1 <= n2 then 1:stack else 0:stack, state)
run (And:code, n2:n1:stack, state) = run (code, if n1 /= 0 && n2 /= 0 then 1:stack else 0:stack, state)
run (Neg:code, n:stack, state) = run (code, if n == 0 then 1:stack else 0:stack, state)
run (Fetch x:code, stack, state) = run (code, val:stack, state)
  where val = case lookup x state of
                Just n -> n
                Nothing -> error "Run-time error"
run (Store x:code, n:stack, state) = run (code, stack, (x, n):state)
run (Noop:code, stack, state) = run (code, stack, state)
run (Branch c1 c2:code, 1:stack, state) = run (c1 ++ code, stack, state)
run (Branch c1 c2:code, 0:stack, state) = run (c2 ++ code, stack, state)
run (Loop c1 c2:code, stack, state) = run (c1 ++ [Branch c2 [Loop c1 c2]] ++ code, stack, state)
run (_, _, _) = error "Run-time error"



-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"
