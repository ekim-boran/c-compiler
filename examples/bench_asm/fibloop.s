    .globl fibonacci_loop
    .section .text
    .type fibonacci_loop @function
fibonacci_loop:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.fibonacci_loop.0:
    li a4,0
    li a5,0
    j .fibonacci_loop.2
.fibonacci_loop.2:
    mv a2,a4
    mv a3,a5
    li a6,10
    slt a4,a3,a6
    beq a4,zero, .fibonacci_loop.5
    mv t1,a1
    mv t2,a1
    li t3,1
    j .fibonacci_loop.7
.fibonacci_loop.5:
    mv a0,a2
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.fibonacci_loop.7:
    mv a4,t1
    mv a5,t2
    mv t0,t3
    slt t1,t0,a0
    beq t1,zero, .fibonacci_loop.10
    j .fibonacci_loop.8
.fibonacci_loop.8:
    add a4,a4,a5
    add t1,a4,a4
    add t1,t1,a4
    add t1,t1,a4
    add t1,t1,a4
    add t1,t1,a4
    sub t1,t1,a4
    sub t1,t1,a4
    sub t1,t1,a4
    sub t1,t1,a4
    sub a4,t1,a4
    addi t0,t0,1
    mv t1,a5
    mv t2,a4
    mv t3,t0
    j .fibonacci_loop.7
.fibonacci_loop.10:
    add a2,a2,a5
    addi a3,a3,1
    mv a4,a2
    mv a5,a3
    j .fibonacci_loop.2
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
    li a0,2000000
    li a1,12
    sd a2,-40(s0)
    call fibonacci_loop
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
