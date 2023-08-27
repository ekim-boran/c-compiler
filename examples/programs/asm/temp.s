    .globl fibonacci
    .section .text
    .type fibonacci @function
fibonacci:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.fibonacci.0:
    add a1,a0,a0
    li a6,0
    xor a1,a1,a6
    snez a1,a1
    beq a1,zero, .fibonacci.5
    j .fibonacci.3
.fibonacci.3:
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.fibonacci.5:
    li a0,0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    li a0,1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
