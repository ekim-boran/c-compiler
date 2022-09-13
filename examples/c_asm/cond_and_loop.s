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
    li a6,100
    rem a2,a0,a6
    li a4,0
    li a3,2
    j .main.8
.main.8:
    mv a0,a4
    mv a1,a3
    slt a3,a0,a2
    beq a3,zero, .main.11
    j .main.9
.main.9:
    li a6,2
    rem a3,a0,a6
    li a6,0
    xor a3,a3,a6
    snez a3,a3
    beq a3,zero, .main.13
    j .main.12
.main.11:
    mv a0,a1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.main.12:
    addi a1,a1,5
    mv a4,a1
    j .main.14
.main.13:
    addi a1,a1,7
    mv a4,a1
    j .main.14
.main.14:
    mv a1,a4
    beq a3,zero, .main.16
    j .main.15
.main.15:
    addi a0,a0,2
    mv a4,a0
    mv a3,a0
    j .main.17
.main.16:
    addi a0,a0,1
    mv a4,a0
    mv a3,a0
    j .main.17
.main.17:
    mv a0,a4
    mv a4,a0
    mv a3,a1
    j .main.8
    .globl nonce
    .section .data
nonce:
    .word  0xc
