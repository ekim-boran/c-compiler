    .globl gcd
    .section .text
    .type gcd @function
gcd:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.gcd.0:
    li a6,0
    slt a2,a6,a0
    beq a2,zero, .gcd.2
    mv a3,a0
    j .gcd.3
.gcd.2:
    negw a0,a0
    mv a3,a0
    j .gcd.3
.gcd.3:
    mv a0,a3
    li a6,0
    slt a2,a6,a1
    beq a2,zero, .gcd.5
    mv a3,a1
    j .gcd.6
.gcd.5:
    negw a1,a1
    mv a3,a1
    j .gcd.6
.gcd.6:
    mv a1,a3
    mv a2,a0
    mv a3,a1
    j .gcd.8
.gcd.8:
    mv a0,a2
    mv a1,a3
    xor a2,a0,a1
    snez a2,a2
    beq a2,zero, .gcd.11
    j .gcd.9
.gcd.9:
    slt a2,a1,a0
    beq a2,zero, .gcd.13
    j .gcd.12
.gcd.11:
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.gcd.12:
    sub a0,a0,a1
    mv a2,a0
    mv a3,a1
    j .gcd.14
.gcd.13:
    sub a1,a1,a0
    mv a2,a0
    mv a3,a1
    j .gcd.14
.gcd.14:
    mv a0,a2
    mv a1,a3
    mv a2,a0
    mv a3,a1
    j .gcd.8
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-32
    sd ra,24(sp)
    sd s0,16(sp)
    addi s0,sp,32
.main.0:
    li a0,18
    li a1,21
    call gcd
    li a6,3
    xor a0,a0,a6
    seqz a0,a0
    ld s0,16(sp)
    ld ra,24(sp)
    addi sp,sp,32
    ret
