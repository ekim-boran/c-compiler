    .globl init
    .section .text
    .type init @function
init:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.init.0:
    li a4,0
    j .init.2
.init.2:
    mv a3,a4
    slt a4,a3,a0
    beq a4,zero, .init.5
    j .init.3
.init.3:
    mv a4,a3
    li a6,20
    mul a4,a4,a6
    add a4,a2,a4
    addi a5,a4,0
    li t0,0
    j .init.7
.init.5:
    li a0,0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.init.7:
    mv a4,t0
    slt t0,a4,a1
    beq t0,zero, .init.10
    j .init.8
.init.8:
    mv t0,a4
    li a6,4
    mul t0,t0,a6
    add t0,a5,t0
    mul t1,a3,a4
    sw t1,0(t0)
    addi a4,a4,1
    mv t0,a4
    j .init.7
.init.10:
    addi a3,a3,1
    mv a4,a3
    j .init.2
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-248
    sd ra,240(sp)
    sd s0,232(sp)
    addi s0,sp,248
.main.0:
    addi a7,s0,-112
    addi a0,a7,4
    addi a0,a0,0
    addi a2,a0,0
    li a0,4
    li a1,5
    sd a3,-248(s0)
    sd a4,-240(s0)
    call init
    ld a3,-248(s0)
    ld a4,-240(s0)
    addi a7,s0,-208
    mv a0,a7
    addi a7,s0,-112
    mv a1,a7
    li a2,96
    sd a4,-248(s0)
    call memcpy
    ld a4,-248(s0)
    addi a7,s0,-208
    addi a0,a7,4
    addi a0,a0,0
    addi a0,a0,0
    addi a0,a0,40
    addi a0,a0,0
    addi a0,a0,12
    lw a0,0(a0)
    li a6,6
    xor a0,a0,a6
    seqz a0,a0
    ld s0,232(sp)
    ld ra,240(sp)
    addi sp,sp,248
    ret
