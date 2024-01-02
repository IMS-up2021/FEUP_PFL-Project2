-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Data.List
import System.IO

import Data.Char

import Text.Parsec hiding (State)
import Text.Parsec.String



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
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

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



-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Aexp =
     CasoBase Integer
    | CasoBase2 String
    | Multiplicacao Aexp Aexp
    | Soma Aexp Aexp
    | Diferenca Aexp Aexp
    deriving Show


-- le, equal, and, not
data Bexp =
    Folha Bool
    | Igual1 Bexp Bexp
    | Igual2 Aexp Aexp
    | And2 Bexp Bexp
    | Not2 Bexp
    | LessEqual Aexp Aexp
    deriving Show

data Stm =
  Assignment String Aexp
  | If2 Bexp [Stm] [Stm]
  | Loop2 Bexp [Stm]
  | Sequence [Stm]
  deriving Show

type Program = [Stm]

compile :: Program -> Code
compile [] = []
compile (Assignment var exp : restantesStatement) = compA exp ++ [Store var] ++ compile restantesStatement
compile (If2 condicao se senao : restantesStatement) = compB condicao ++ [Branch (compile se) (compile senao)] ++ compile restantesStatement
compile (Loop2 condicao ciclo : restantesStatement) = Loop (compB condicao) (compile ciclo) : compile restantesStatement
compile (Sequence sequencia : restantesStatement) = compile sequencia ++ compile restantesStatement


compB :: Bexp -> Code

compB (Folha True) = [Tru]
compB (Folha False) = [Fals]
compB (Igual1 left right) = compB right ++ compB left ++ [Equ]
compB (Igual2 left right) = compA right ++ compA left ++ [Equ]
compB (And2 left right) = compB right ++ compB left ++ [And]
compB (Not2 exp) = compB exp ++ [Neg]
compB (LessEqual left right) = compA right ++ compA left ++ [Le]

compA :: Aexp -> Code

compA (CasoBase int) = [Push int]
compA (CasoBase2 var) = [Fetch var]
compA (Multiplicacao left right) = compA right ++ compA left ++ [Mult]
compA (Soma left right) = compA right ++ compA left ++ [Add]
compA (Diferenca left right) = compA right ++ compA left ++ [Sub]


lexer :: String -> [String]
lexer [] = []
lexer ('+' : restStr) = "+" : lexer restStr
lexer ('*' : restStr) = "*" : lexer restStr
lexer ('-' : restStr) = "-" : lexer restStr
lexer ('(' : restStr) = "(" : lexer restStr
lexer (')' : restStr) = ")" : lexer restStr


lexer (';' : rest) = ";" : lexer rest
lexer (':' : '=' : rest) = ":=" : lexer rest
lexer ('<' : '=' : rest) = "<=" : lexer rest
lexer ('=' : '=' : rest) = "==" : lexer rest
lexer ('=' : rest) = "=" : lexer rest

-- If statements
lexer ('i' : 'f' : rest) = "if" : lexer rest
lexer ('t' : 'h' : 'e': 'n' : rest) = "then" : lexer rest
lexer ('e' : 'l' : 's': 'e' : rest) = "else" : lexer rest

-- Boolean operations
lexer ('n' : 'o' : 't': rest) = "not" : lexer rest
lexer ('a' : 'n' : 'd': rest) = "and" : lexer rest

-- While loops
lexer ('w' : 'h' : 'i': 'l': 'e' : rest) = "while" : lexer rest
lexer ('d' : 'o' : rest) = "do" : lexer rest

-- Boolean values
lexer ('T' : 'r' : 'u': 'e' : rest) = "true" : lexer rest
lexer ('F' : 'a' : 'l': 's': 'e' : rest) = "false" : lexer rest

-- Space
lexer (chr : rest)
    | isSpace chr
    = lexer rest

-- Integer values
lexer str@(chr : _)
    | isDigit chr
    = integer : lexer rest
    where
      (integer, rest) = span isDigit str

-- Variables
lexer str@(chr : _)
    | isLower chr
    = variable : lexer rest
    where
      (variable, rest) = span isVariable str
      isVariable :: Char -> Bool
      isVariable c = not (isSpace c) && isAlphaNum c && c `notElem` ['+', '-', '*', ':', '=', ';', '<', ')', '(']


divide :: [String] -> [[String]]
divide [] = []
divide list =
  let (before, after) = break (== ";") list
  in case after of
    [] -> [before]
    (_:rest) -> before : divide rest

-- parse :: String -> Program
parse :: String -> Program
--parse input = buildData (divide (lexer input))
parse = undefined 

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (Main.parse programCode), createEmptyStack, createEmptyState)



-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
