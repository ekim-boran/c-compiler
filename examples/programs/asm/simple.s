    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    la a7,nonce
    lw a0,0(a7)
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl nonce
    .section .data
nonce:
    .word  0xc
