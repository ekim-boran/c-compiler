    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    la a7,temp
    li a6,61238
    sb a6,0(a7)
    li a0,61238
    li a6,2
    slt a0,a0,a6
    xori a0,a0,1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl temp
    .section .data
temp:
    .byte  0x0
