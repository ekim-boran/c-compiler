    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    li a6,1
    negw a1,a6
    andi a3,a1,255
    li a6,128
    negw a0,a6
    andi a2,a0,255
    or a0,a2,a3
    and a5,a2,a3
    li a6,127
    and a4,a2,a6
    li a6,127
    or a3,a2,a6
    xor a2,a1,a1
    andi a2,a2,255
    li a6,0
    xor a1,a1,a6
    andi a1,a1,255
    li a6,255
    xor a0,a0,a6
    seqz a0,a0
    li t0,0
    beq a0,zero, .main.15
    j .main.13
.main.1:
    mv a0,a1
    li a6,255
    xor a0,a0,a6
    seqz a0,a0
    mv a2,a0
    j .main.3
.main.3:
    mv a0,a2
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.main.4:
    mv a0,a2
    li a6,0
    xor a0,a0,a6
    seqz a0,a0
    mv a3,a0
    j .main.6
.main.6:
    mv a0,a3
    li a2,0
    beq a0,zero, .main.3
    j .main.1
.main.7:
    mv a0,a3
    li a6,255
    xor a0,a0,a6
    seqz a0,a0
    mv a4,a0
    j .main.9
.main.9:
    mv a0,a4
    li a3,0
    beq a0,zero, .main.6
    j .main.4
.main.10:
    mv a0,a4
    li a6,0
    xor a0,a0,a6
    seqz a0,a0
    mv a5,a0
    j .main.12
.main.12:
    mv a0,a5
    li a4,0
    beq a0,zero, .main.9
    j .main.7
.main.13:
    mv a0,a5
    li a6,128
    xor a0,a0,a6
    seqz a0,a0
    mv t0,a0
    j .main.15
.main.15:
    mv a0,t0
    li a5,0
    beq a0,zero, .main.12
    j .main.10
