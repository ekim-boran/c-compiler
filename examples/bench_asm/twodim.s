    .globl two_dimension_array
    .section .text
    .type two_dimension_array @function
two_dimension_array:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.two_dimension_array.0:
    li a6,10000
    slt a2,a6,a0
    xori a2,a2,1
    seqz a2,a2
    li a3,0
    beq a2,zero, .two_dimension_array.5
    j .two_dimension_array.1
.two_dimension_array.1:
    mv a0,a1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.two_dimension_array.5:
    mv a2,a3
    slt a3,a2,a0
    li a4,0
    beq a3,zero, .two_dimension_array.10
    j .two_dimension_array.6
.two_dimension_array.6:
    la a7,two_dimension_array_arr
    addi a3,a7,0
    mv a4,a2
    li a6,4
    mul a4,a4,a6
    add a3,a3,a4
    add a4,a2,a1
    sw a4,0(a3)
    addi a2,a2,1
    mv a3,a2
    j .two_dimension_array.5
.two_dimension_array.10:
    mv a1,a4
    slt a2,a1,a0
    li a3,0
    li a4,0
    beq a2,zero, .two_dimension_array.20
    j .two_dimension_array.11
.two_dimension_array.11:
    mv a2,a1
    li a6,4
    mul a3,a2,a6
    li a4,0
    j .two_dimension_array.15
.two_dimension_array.15:
    mv a2,a4
    slt a4,a2,a0
    beq a4,zero, .two_dimension_array.18
    j .two_dimension_array.16
.two_dimension_array.16:
    la a7,two_dimension_array_arr
    addi a4,a7,0
    add a5,a4,a3
    lw t0,0(a5)
    mv t1,a2
    li a6,4
    mul t1,t1,a6
    add a4,a4,t1
    lw a4,0(a4)
    add a4,t0,a4
    sw a4,0(a5)
    addi a2,a2,1
    mv a4,a2
    j .two_dimension_array.15
.two_dimension_array.18:
    addi a1,a1,1
    mv a4,a1
    j .two_dimension_array.10
.two_dimension_array.20:
    mv a1,a3
    mv a2,a4
    slt a3,a2,a0
    beq a3,zero, .two_dimension_array.23
    j .two_dimension_array.21
.two_dimension_array.21:
    la a7,two_dimension_array_arr
    addi a3,a7,0
    mv a4,a2
    li a6,4
    mul a4,a4,a6
    add a3,a3,a4
    lw a3,0(a3)
    add a1,a1,a3
    addi a2,a2,1
    mv a3,a1
    mv a4,a2
    j .two_dimension_array.20
.two_dimension_array.23:
    mv a0,a1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-40
    sd ra,32(sp)
    sd s0,24(sp)
    addi s0,sp,40
.main.0:
    call clock
    mv a2,a0
    li a0,10000
    li a1,33
    sd a2,-40(s0)
    call two_dimension_array
    ld a2,-40(s0)
    mv a3,a0
    sd a2,-40(s0)
    sw a3,-32(s0)
    call clock
    ld a2,-40(s0)
    lw a3,-32(s0)
    sub a1,a0,a2
    la a7,.temp0
    mv a0,a7
    sw a3,-40(s0)
    call printf
    lw a3,-40(s0)
    mv a0,a3
    ld s0,24(sp)
    ld ra,32(sp)
    addi sp,sp,40
    ret
    .globl two_dimension_array_arr
    .section .data
two_dimension_array_arr:
    .zero  40000
    .globl .temp0
    .section .data
.temp0:
    .asciz  "%d"
