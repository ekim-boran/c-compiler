    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    li a0,0
    li a1,4294967163
    xor a0,a0,a1
    andi a0,a0,255
    li a6,123
    xor a0,a0,a6
    seqz a0,a0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
