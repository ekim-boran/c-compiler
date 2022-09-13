    .globl foo
    .section .text
    .type foo @function
foo:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.foo.0:
    add a0,a0,a1
    add a0,a0,a2
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl foo2
    .section .text
    .type foo2 @function
foo2:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.foo2.0:
    la a7,foo
    mv a0,a7
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl foo3
    .section .text
    .type foo3 @function
foo3:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.foo3.0:
    la a7,foo2
    mv a0,a7
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-48
    sd ra,40(sp)
    sd s0,32(sp)
    addi s0,sp,48
.main.0:
    call foo3
    jalr a0
    mv a3,a0
    li a0,2
    li a1,2
    li a2,2
    jalr a3
    li a6,6
    xor a0,a0,a6
    seqz a0,a0
    ld s0,32(sp)
    ld ra,40(sp)
    addi sp,sp,48
    ret
