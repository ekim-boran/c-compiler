    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    li a2,0
    li a3,0
    j .main.2
.main.2:
    mv a0,a2
    mv a1,a3
    li a6,10
    slt a2,a1,a6
    beq a2,zero, .main.5
    j .main.3
.main.3:
    li a6,4
    xor a2,a1,a6
    seqz a2,a2
    mv a4,a0
    li a5,0
    beq a2,zero, .main.10
    mv a3,a0
    j .main.4
.main.4:
    mv a0,a3
    addi a1,a1,1
    mv a2,a0
    mv a3,a1
    j .main.2
.main.5:
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.main.10:
    mv a0,a4
    mv a2,a5
    li a6,10
    slt a4,a2,a6
    mv a3,a0
    beq a4,zero, .main.4
    j .main.11
.main.11:
    add a3,a1,a2
    add a0,a0,a3
    li a6,3
    xor a4,a3,a6
    seqz a4,a4
    mv a5,a0
    beq a4,zero, .main.16
    j .main.14
.main.14:
    add a0,a0,a3
    mv a5,a0
    j .main.16
.main.16:
    mv a0,a5
    addi a2,a2,1
    mv a4,a0
    mv a5,a2
    j .main.10
