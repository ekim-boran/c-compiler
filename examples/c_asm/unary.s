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
    li a6,1
    sub a0,a0,a6
    andi a0,a0,255
    li a6,1
    slt a0,a0,a6
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
