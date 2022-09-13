    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-36
    sd ra,28(sp)
    sd s0,20(sp)
    addi s0,sp,36
.main.0:
    addi a7,s0,-36
    addi a1,a7,0
    addi a2,a1,0
    li a6,1
    sw a6,0(a2)
    addi a2,a1,4
    li a6,2
    sw a6,0(a2)
    addi a2,a1,8
    li a6,3
    sw a6,0(a2)
    addi a2,a1,12
    li a6,4
    sw a6,0(a2)
    addi a1,a1,16
    li a6,5
    negw a2,a6
    sw a2,0(a1)
    li a3,0
    li a4,0
    j .main.2
.main.2:
    mv a1,a3
    mv a2,a4
    li a6,5
    slt a3,a2,a6
    beq a3,zero, .main.5
    j .main.3
.main.3:
    addi a7,s0,-36
    addi a3,a7,0
    mv a4,a2
    li a6,4
    mul a4,a4,a6
    add a3,a3,a4
    lw a3,0(a3)
    add a1,a1,a3
    la a7,g_a
    addi a3,a7,0
    add a3,a3,a4
    lw a3,0(a3)
    add a1,a1,a3
    addi a2,a2,1
    mv a3,a1
    mv a4,a2
    j .main.2
.main.5:
    mv a0,a1
    ld s0,20(sp)
    ld ra,28(sp)
    addi sp,sp,36
    ret
    .globl g_a
    .section .data
g_a:
    .word  0x1
    .word  0x2
    .word  0x3
    .zero  8
