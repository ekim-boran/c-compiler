    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-56
    sd ra,48(sp)
    sd s0,40(sp)
    addi s0,sp,56
.main.0:
    addi a7,s0,-56
    addi a2,a7,0
    mv a4,a2
    li a1,0
    j .main.2
.main.2:
    mv a0,a4
    li a6,10
    slt a3,a1,a6
    beq a3,zero, .main.5
    j .main.3
.main.3:
    addi a3,a0,4
    sw a1,0(a0)
    addi a0,a1,1
    mv a4,a3
    mv a1,a0
    j .main.2
.main.5:
    addi a0,a2,20
    lw a0,0(a0)
    li a6,5
    xor a0,a0,a6
    seqz a0,a0
    ld s0,40(sp)
    ld ra,48(sp)
    addi sp,sp,56
    ret
