    .globl fibonacci_recursive
    .section .text
    .type fibonacci_recursive @function
fibonacci_recursive:
    addi sp,sp,-48
    sd ra,40(sp)
    sd s0,32(sp)
    addi s0,sp,48
.fibonacci_recursive.0:
    mv a2,a0
    mv a3,a1
    li a6,2
    slt a0,a2,a6
    beq a0,zero, .fibonacci_recursive.2
    j .fibonacci_recursive.1
.fibonacci_recursive.1:
    mv a0,a3
    ld s0,32(sp)
    ld ra,40(sp)
    addi sp,sp,48
    ret
.fibonacci_recursive.2:
    li a6,1
    sub a0,a2,a6
    mv a1,a3
    sw a2,-48(s0)
    sw a3,-40(s0)
    call fibonacci_recursive
    lw a2,-48(s0)
    lw a3,-40(s0)
    mv a4,a0
    li a6,2
    sub a0,a2,a6
    mv a1,a3
    sw a4,-48(s0)
    call fibonacci_recursive
    lw a4,-48(s0)
    add a0,a4,a0
    ld s0,32(sp)
    ld ra,40(sp)
    addi sp,sp,48
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
    li a0,36
    li a1,12
    sd a2,-40(s0)
    call fibonacci_recursive
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
    .globl .temp0
    .section .data
.temp0:
    .asciz  "%d"
