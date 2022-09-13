    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    la a7,k
    lw a0,0(a7)
    li a6,1
    add a0,a6,a0
    li a1,12
    li a6,0
    beq a6,a0, .main.1
    li a1,13
    li a6,1
    beq a6,a0, .main.1
    li a1,14
    j .main.1
.main.1:
    mv a0,a1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl k
    .section .data
k:  .word  0xb
