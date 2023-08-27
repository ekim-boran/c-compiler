    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-56
    sd ra,48(sp)
    sd s0,40(sp)
    addi s0,sp,56
.main.0:
    li a0,16
    call malloc
    mv a3,a0
    mv a4,a3
    li a2,0
    j .main.2
.main.2:
    mv a1,a4
    li a6,1000
    slt a0,a2,a6
    li a4,0
    mv a5,a3
    beq a0,zero, .main.7
    j .main.3
.main.3:
    addi a4,a1,8
    li a0,16
    sd a1,-56(s0)
    sw a2,-48(s0)
    sd a3,-40(s0)
    sd a4,-32(s0)
    call malloc
    ld a1,-56(s0)
    lw a2,-48(s0)
    ld a3,-40(s0)
    ld a4,-32(s0)
    sd a0,0(a4)
    addi a0,a1,0
    sw a2,0(a0)
    ld a0,0(a4)
    addi a1,a2,1
    mv a4,a0
    mv a2,a1
    j .main.2
.main.7:
    mv a0,a4
    mv a1,a5
    addi a2,a1,8
    ld a3,0(a2)
    li a6,0
    xor a3,a3,a6
    snez a3,a3
    beq a3,zero, .main.10
    j .main.8
.main.8:
    addi a1,a1,0
    lw a1,0(a1)
    add a0,a0,a1
    ld a1,0(a2)
    mv a4,a0
    mv a5,a1
    j .main.7
.main.10:
    ld s0,40(sp)
    ld ra,48(sp)
    addi sp,sp,56
    ret
    .globl asd
    .section .text
    .type asd @function
asd:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.asd.0:
    li a0,1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
