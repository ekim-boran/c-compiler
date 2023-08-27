    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    li a3,0
    li a2,0
    j .main.2
.main.2:
    mv a0,a3
    mv a1,a2
    li a6,11
    slt a2,a0,a6
    beq a2,zero, .main.5
    j .main.3
.main.3:
    add a1,a1,a0
    addi a0,a0,1
    mv a3,a0
    mv a2,a1
    j .main.2
.main.5:
    li a6,55
    xor a0,a1,a6
    seqz a0,a0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
