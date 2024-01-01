# Second PFL Project - Haskell

## T10_G04
- Catarina Isabel Moreira Canelas up202103628 (50%)
- Inês Martin Soares up202108852 (50%)

## Project Description

This project involves creating a virtual machine and a compiler for a simple imperative programming language. The machine operates with a set of instructions and has a stack-based evaluation system. The imperative language includes arithmetic and boolean expressions, assignments, sequences of statements, if-then-else statements, and while loops.

### Part 1: Virtual Machine
(a) Define Types
- Stack: Create a type to represent the machine's stack.
- State: Define a type to represent the machine's state.

(b) Implement Functions
- createEmptyStack: Create an empty machine's stack.
- createEmptyState: Create an empty machine's state.
- stack2Str: Convert a stack to a string.
- state2Str: Convert a machine state to a string.
- run: Implement an interpreter for programs, given a list of instructions, stack, and initial storage.

### Part 2: Compiler
(a) Define Types
- Aexp: Represent arithmetic expressions.
- Bexp: Represent boolean expressions.
- Stm: Represent statements.

(b) Implement Compiler
- compile: Compiler function that translates a program in the imperative language into a list of machine instructions.
- compA: Compile arithmetic expressions.
- compB: Compile boolean expressions.

(c) Implement Parser
- parse: Create a parser that transforms an imperative program (given as a string) into its representation in the Stm data structure.


## Code Description

### Data Types
- **Inst**: Represents instructions for the virtual machine.
- **Code**: A list of instructions.
- **Stack**: A list of integers representing the evaluation stack.
- **State**: A list of pairs representing the storage state where variables are associated with integer values.

### Helper Functions
- `createEmptyStack`: Returns an empty stack.
- `stack2Str`: Converts a stack to a string for display purposes.
- `createEmptyState`: Returns an empty storage state.
- `state2Str`: Converts a storage state to a string for display purposes.

### Interpreter Function: run
- `run`: Takes a tuple `(Code, Stack, State)` and executes the virtual machine instructions until the code is empty. It returns a tuple containing the remaining code, the resulting stack, and the updated state.

### Interpreter Execution
- The `run` function pattern matches on different instructions and executes them accordingly.
- Arithmetic and boolean operations manipulate the stack.
- `Fetch` retrieves the value of a variable from the state and pushes it onto the stack.
- `Store` updates the value of a variable in the state.
- `Noop` is a dummy instruction that does nothing.
- `Branch` and `Loop` control flow instructions determine the next set of instructions based on the top of the stack.

### Error Handling
- The interpreter includes basic error handling. For example, attempting to fetch a variable not in the state results in a run-time error.

#### Example of instruction (part 1)
```haskell
testAssembler [Push 3, Push 4, Add, Store "result", Noop]
```
- Pushes 3 and 4 onto the stack, adds them, stores the result in the state.

### Lexer
The lexer function is a lexical analyzer that converts a string into a list of tokens. Tokens include arithmetic operators, parentheses, semicolons, assignment operators, comparison operators, keywords (if, then, else, not, and, while, do), boolean values (true, false), integers, and variables.

#### Examples
Ex.1:
```haskell
y := 1; while not (x = 1) do (y := y * x; x := x - 1)
```
- This is lexed into a list of tokens l1.

Ex.2:
```haskell
x := 2; y := (x - 3)*(4 + 2*3); z := x + x*(2);
```
- This is lexed into a list of tokens l2.

### Parser (Not Implemented yet)
The provided lexer can be used as a foundation for implementing a parser. A parser would be responsible for converting the list of tokens into a structured representation of the program. This representation could be an abstract syntax tree (AST) that corresponds to the syntax and structure of the imperative programming language.

### Compiler
The compile function takes a program (represented as a list of statements) and translates it into machine instructions. It uses helper functions compA and compB for compiling arithmetic and boolean expressions, respectively.

#### Example Compilation
```haskell
compile l1
```
- Compiles the first example program (l1) into a list of machine instructions.

### Conclusion
- The project demonstrates the implementation of a simple programming language interpreter and compiler, providing insights into lexing, parsing, and execution of imperative programs.
- While the current implementation is limited in its scope, it serves as a foundational structure that can be extended to support additional language features, optimizations, and error handling.
- Further development could involve enhancing the parser to construct an abstract syntax tree (AST), enabling more sophisticated analyses and optimizations.
- In summary, this project serves as a practical exploration of language processing concepts and provides a starting point for more advanced language development and compiler construction. The codebase can be expanded and refined to accommodate richer language constructs and optimizations, offering a valuable learning experience in the realm of language design and implementation.