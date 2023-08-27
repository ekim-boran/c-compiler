    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    li a6,6
    slli a0,a6,32
    srli a0,a0,32
    li a1,4
    add a0,a0,a1
    li a1,5
    add a0,a0,a1
    addi a0,a0,6
    li a1,7
    add a0,a0,a1
    li a6,28
    xor a0,a0,a6
    seqz a0,a0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
