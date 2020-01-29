# Brainfuck scripting language compiler

Compiler written in Haskell to compile a custom (very basic) scripting language to [BrainFuck](https://en.wikipedia.org/wiki/Brainfuck).

### Example

The following example program reads one character from the input and prints the next character in the alphabet.

Input

    inputChar = read()
    
    if(inputChar) {
        print(add(inputChar, 1))
    }
   
Output

    ,><[->+>+<<]>>[-<<+>>]<[><<[->>+>+<<<]>>>[-<<<+>>>]<>+[-<+>]<.[-][-]<[-]][-]<[-]
    
### Usage

Running requires [stack](https://docs.haskellstack.org/en/stable/README/) to be installed on the system.

To compile the program in the file test.bf, execute the following command

    > stack run test.bf
    

