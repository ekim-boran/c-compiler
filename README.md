# toy c-compiler

- Tests require `riscv64-linux-gnu-gcc` and `qemu-riscv64-static`
- Test: stack test
   * It compiles and runs all files in ./examples/c and ./examples/bench and compares their output with gcc-riscv compiler output 
   * Generated assemblies are in `examples/bench/asm` `examples/programs/asm`

### Todo: 

- FIX assembly generation (src/AsmGen). 
- FIX struct conversion part (/irgen/structs.hs)
- front end: Add error handling/messages:  If it encounters anything that are not supported, it gives an exception or produces wrong output
- front end: fix typecasting rules: Ex: what to do when adding unsigned char with unsigned short, 
- tests: fix optimization tests    
- infra: write ir interpreter to test ir passes
- add more optimizations, current optimizations are in /src/Opt folder

<pre>
 Filename             |GCC Output|Output|GCC -O0 Execution Time|Execution Time|% Faster
_a.c                  |59        |59    |-                     |-             |-                             
_linkedlist.c         |44        |44    |-                     |-             |-                             
_struct.c             |1         |1     |-                     |-             |-                             
_struct1.c            |234       |234   |-                     |-             |-                             
_struct2.c            |1         |1     |-                     |-             |-                             
_struct3.c            |1         |1     |-                     |-             |-                             
_struct4.c            |111       |111   |-                     |-             |-                             
alignof.c             |1         |1     |-                     |-             |-                             
array.c               |1         |1     |-                     |-             |-                             
array2.c              |1         |1     |-                     |-             |-                             
array3.c              |1         |1     |-                     |-             |-                             
array4.c              |1         |1     |-                     |-             |-                             
array5.c              |11        |11    |-                     |-             |-                             
bar.c                 |1         |1     |-                     |-             |-                             
bitwise.c             |1         |1     |-                     |-             |-                             
comma.c               |1         |1     |-                     |-             |-                             
complete_cond.c       |1         |1     |-                     |-             |-                             
cond.c                |1         |1     |-                     |-             |-                             
cond_and_loop.c       |39        |39    |-                     |-             |-                             
example_prog.c        |88        |88    |-                     |-             |-                             
fib2.c                |1         |1     |-                     |-             |-                             
fib3.c                |1         |1     |-                     |-             |-                             
fib4.c                |1         |1     |-                     |-             |-                             
fib5.c                |1         |1     |-                     |-             |-                             
fibonacci.c           |144       |144   |-                     |-             |-                             
float.c               |1         |1     |-                     |-             |-                             
float2.c              |109       |109   |-                     |-             |-                             
float3.c              |216       |216   |-                     |-             |-                             
foo.c                 |1         |1     |-                     |-             |-                             
foo2.c                |1         |1     |-                     |-             |-                             
foo3.c                |32        |32    |-                     |-             |-                             
foo4.c                |1         |1     |-                     |-             |-                             
for_continue_break.c  |1         |1     |-                     |-             |-                             
gcd.c                 |1         |1     |-                     |-             |-                             
integer_literal.c     |1         |1     |-                     |-             |-                             
integer_literal2.c    |1         |1     |-                     |-             |-                             
logical_op.c          |1         |1     |-                     |-             |-                             
minus_constant.c      |1         |1     |-                     |-             |-                             
negate.c              |1         |1     |-                     |-             |-                             
pointer.c             |1         |1     |-                     |-             |-                             
return_void.c         |1         |1     |-                     |-             |-                             
simple.c              |12        |12    |-                     |-             |-                             
simple_cond.c         |1         |1     |-                     |-             |-                             
simple_for.c          |1         |1     |-                     |-             |-                             
simple_if.c           |1         |1     |-                     |-             |-                             
sizeof.c              |1         |1     |-                     |-             |-                             
sizeof2.c             |1         |1     |-                     |-             |-                             
switch.c              |14        |14    |-                     |-             |-                             
temp.c                |1         |1     |-                     |-             |-                             
temp2.c               |7         |7     |-                     |-             |-                             
test.c                |1         |1     |-                     |-             |-                             
typecast.c            |1         |1     |-                     |-             |-                             
typedef.c             |1         |1     |-                     |-             |-                             
unary.c               |0         |0     |-                     |-             |-                             
while_continue_break.c|79        |79    |-                     |-             |-                             
exotic.c              |44        |44    |-                     |-             |-                             
fibloop.c             |240       |240   |495390                |49229         |906
fibloop2.c            |240       |240   |88042                 |40940         |115
fibrec.c              |172       |172   |401065                |326396        |22
graph_dijkstra.c      |128       |128   |82909                 |65854         |25
graph_floyd.c         |139       |139   |7741744               |3992580       |93
matrix.c              |1         |1     |686182                |328565        |108
twodim.c              |120       |120   |314632                |212372        |48
</pre>