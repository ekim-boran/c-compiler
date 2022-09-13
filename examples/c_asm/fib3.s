    .globl fibonacci
    .section .text
    .type fibonacci @function
fibonacci:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.fibonacci.0:
    li a6,2
    slt a1,a0,a6
    li t0,1
    li a5,0
    li a4,1
    beq a1,zero, .fibonacci.5
    j .fibonacci.1
.fibonacci.1:
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.fibonacci.5:
    mv a1,t0
    mv a2,a5
    mv a3,a4
    slt a4,a1,a0
    beq a4,zero, .fibonacci.8
    j .fibonacci.6
.fibonacci.6:
    add a2,a2,a3
    addi a1,a1,1
    mv t0,a1
    mv a5,a3
    mv a4,a2
    j .fibonacci.5
.fibonacci.8:
    mv a0,a3
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-24
    sd ra,16(sp)
    sd s0,8(sp)
    addi s0,sp,24
.main.0:
    li a0,9
    call fibonacci
    li a6,34
    xor a0,a0,a6
    seqz a0,a0
    ld s0,8(sp)
    ld ra,16(sp)
    addi sp,sp,24
    ret
