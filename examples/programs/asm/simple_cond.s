    .globl f
    .section .text
    .type f @function
f:  addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.f.0:
    addi a0,a0,8
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
    li a0,1
    call f
    li a6,9
    xor a0,a0,a6
    seqz a0,a0
    ld s0,8(sp)
    ld ra,16(sp)
    addi sp,sp,24
    ret
