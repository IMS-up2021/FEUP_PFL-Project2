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