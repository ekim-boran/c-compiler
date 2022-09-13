# toy c-compiler


- Usage: stack run -- -c example_prog.c [-q|--executable]
- Test: stack test
   * it compiles and runs all files in ./examples/c and ./examples/bench and compares their output with gcc-riscv compiler output 



### Todo: 

- register allocation: Add stack spilling
- front end: Add error handling/messages:  If it encounters anything that are not supported, it gives an exception or produces wrong output
- front end: fix typecasting rules: what to do when adding unsigned char with unsigned short
- tests: fix optimization tests   
- infra: port ir interpreter to haskell
- reimplement assembly generation and struct conversion parts (/irgen/structs.hs)
- add more optimizations, current optimizations are in /src/Opt folder
