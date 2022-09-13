    .globl sum
    .section .text
    .type sum @function
sum:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.sum.0:
    li a4,0
    li a5,0
    j .sum.2
.sum.2:
    mv a2,a4
    mv a3,a5
    slt a4,a3,a0
    beq a4,zero, .sum.5
    j .sum.3
.sum.3:
    mv a4,a3
    li a6,4
    mul a4,a4,a6
    add a4,a1,a4
    lw a4,0(a4)
    add a2,a2,a4
    addi a3,a3,1
    mv a4,a2
    mv a5,a3
    j .sum.2
.sum.5:
    mv a0,a2
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-52
    sd ra,44(sp)
    sd s0,36(sp)
    addi s0,sp,52
.main.0:
    li a2,0
    j .main.2
.main.2:
    mv a1,a2
    li a6,5
    slt a2,a1,a6
    beq a2,zero, .main.5
    j .main.3
.main.3:
    addi a7,s0,-36
    addi a2,a7,0
    mv a3,a1
    li a6,4
    mul a3,a3,a6
    add a2,a2,a3
    sw a1,0(a2)
    addi a1,a1,1
    mv a2,a1
    j .main.2
.main.5:
    addi a7,s0,-36
    addi a1,a7,0
    li a0,5
    call sum
    li a6,10
    xor a0,a0,a6
    seqz a0,a0
    ld s0,36(sp)
    ld ra,44(sp)
    addi sp,sp,52
    ret
