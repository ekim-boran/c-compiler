    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    li a1,0
    j .main.2
.main.2:
    mv a0,a1
    li a6,10
    slt a1,a0,a6
    beq a1,zero, .main.5
    j .main.3
.main.3:
    addi a0,a0,1
    mv a1,a0
    j .main.2
.main.5:
    li a0,1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
