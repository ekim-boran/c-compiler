    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-24
    sd ra,16(sp)
    sd s0,8(sp)
    addi s0,sp,24
.main.0:
    li a1,0
    addi a1,a1,1
    slli a1,a1,32
    srli a1,a1,32
    addi a1,a1,1
    slli a1,a1,32
    srli a1,a1,32
    addi a7,s0,-24
    addi a2,a7,0
    li a6,1
    sw a6,0(a2)
    addi a7,s0,-24
    addi a0,a7,4
    li a6,2
    sb a6,0(a0)
    lb a2,0(a0)
    add a1,a1,a2
    lb a0,0(a0)
    add a0,a1,a0
    mv a2,a0
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
    li a6,2
    xor a2,a1,a6
    seqz a2,a2
    li a3,0
    beq a2,zero, .main.11
    li a3,1
    j .main.11
.main.5:
    li a1,0
    li a6,1
    beq a6,a0, .main.12
    mv a1,a0
    j .main.12
.main.7:
    add a0,a0,a1
    addi a1,a1,1
    mv a2,a0
    mv a3,a1
    j .main.2
.main.11:
    mv a2,a3
    beq a2,zero, .main.7
    j .main.5
.main.12:
    mv a0,a1
    ld s0,8(sp)
    ld ra,16(sp)
    addi sp,sp,24
    ret
