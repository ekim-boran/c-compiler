    .globl graph_weight_init
    .section .text
    .type graph_weight_init @function
graph_weight_init:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.graph_weight_init.0:
    addi a5,a1,1
    li a4,0
    j .graph_weight_init.2
.graph_weight_init.2:
    mv a1,a4
    slt a4,a1,a0
    beq a4,zero, .graph_weight_init.5
    j .graph_weight_init.3
.graph_weight_init.3:
    mv a4,a1
    li a6,4000
    mul t0,a4,a6
    add t0,a3,t0
    addi t0,t0,0
    li a6,4
    mul a4,a4,a6
    add a4,t0,a4
    li a6,0
    sw a6,0(a4)
    li t1,1
    j .graph_weight_init.7
.graph_weight_init.5:
    li a0,0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.graph_weight_init.7:
    mv a4,t1
    slt t1,a4,a0
    beq t1,zero, .graph_weight_init.10
    j .graph_weight_init.8
.graph_weight_init.8:
    add t1,a1,a4
    rem t1,t1,a0
    li a6,4
    mul t1,t1,a6
    add t1,t0,t1
    lw t2,0(a2)
    addi t2,t2,1
    sw t2,0(a2)
    sw t2,0(t1)
    lw t1,0(a2)
    rem t1,t1,a5
    li a6,0
    xor t1,t1,a6
    snez t1,t1
    beq t1,zero, .graph_weight_init.13
    j .graph_weight_init.11
.graph_weight_init.10:
    addi a1,a1,1
    mv a4,a1
    j .graph_weight_init.2
.graph_weight_init.11:
    lw t1,0(a2)
    addi t1,t1,1
    sw t1,0(a2)
    j .graph_weight_init.13
.graph_weight_init.13:
    addi a4,a4,1
    mv t1,a4
    j .graph_weight_init.7
    .globl graph_floyd_warshall
    .section .text
    .type graph_floyd_warshall @function
graph_floyd_warshall:
    addi sp,sp,-68
    sd ra,60(sp)
    sd s0,52(sp)
    addi s0,sp,68
.graph_floyd_warshall.0:
    mv a4,a0
    li a6,1000
    slt a0,a6,a4
    xori a0,a0,1
    seqz a0,a0
    beq a0,zero, .graph_floyd_warshall.2
    j .graph_floyd_warshall.1
.graph_floyd_warshall.1:
    mv a0,a1
    ld s0,52(sp)
    ld ra,60(sp)
    addi sp,sp,68
    ret
.graph_floyd_warshall.2:
    addi a7,s0,-20
    li a6,0
    sw a6,0(a7)
    la a7,graph_weight
    addi a5,a7,0
    mv a0,a4
    addi a7,s0,-20
    mv a2,a7
    mv a3,a5
    sw a4,-68(s0)
    sd a5,-60(s0)
    call graph_weight_init
    lw a4,-68(s0)
    ld a5,-60(s0)
    li a6,1
    negw t0,a6
    li a1,0
    j .graph_floyd_warshall.5
.graph_floyd_warshall.5:
    mv a0,a1
    slt a1,a0,a4
    li a2,0
    li a3,0
    beq a1,zero, .graph_floyd_warshall.32
    j .graph_floyd_warshall.6
.graph_floyd_warshall.6:
    mv a1,a0
    li a6,4
    mul t1,a1,a6
    li a6,4000
    mul a1,a1,a6
    add a1,a5,a1
    addi t2,a1,0
    li a2,0
    j .graph_floyd_warshall.10
.graph_floyd_warshall.10:
    mv a1,a2
    slt a2,a1,a4
    beq a2,zero, .graph_floyd_warshall.13
    j .graph_floyd_warshall.11
.graph_floyd_warshall.11:
    mv a2,a1
    li a6,4000
    mul a2,a2,a6
    add a2,a5,a2
    addi t3,a2,0
    add t4,t3,t1
    lw a2,0(t4)
    xor a2,a2,t0
    seqz a2,a2
    li a3,0
    beq a2,zero, .graph_floyd_warshall.18
    j .graph_floyd_warshall.12
.graph_floyd_warshall.12:
    addi a1,a1,1
    mv a2,a1
    j .graph_floyd_warshall.10
.graph_floyd_warshall.13:
    addi a0,a0,1
    mv a1,a0
    j .graph_floyd_warshall.5
.graph_floyd_warshall.18:
    mv a2,a3
    slt a3,a2,a4
    beq a3,zero, .graph_floyd_warshall.12
    j .graph_floyd_warshall.19
.graph_floyd_warshall.19:
    mv a3,a2
    li a6,4
    mul a3,a3,a6
    add t5,t2,a3
    lw t6,0(t5)
    xor t6,t6,t0
    seqz t6,t6
    beq t6,zero, .graph_floyd_warshall.23
    j .graph_floyd_warshall.20
.graph_floyd_warshall.20:
    addi a2,a2,1
    mv a3,a2
    j .graph_floyd_warshall.18
.graph_floyd_warshall.23:
    lw t6,0(t4)
    lw t5,0(t5)
    add t5,t6,t5
    add t6,t3,a3
    lw a3,0(t6)
    xor a3,a3,t0
    snez a3,a3
    li s1,0
    beq a3,zero, .graph_floyd_warshall.30
    j .graph_floyd_warshall.28
.graph_floyd_warshall.26:
    sw t5,0(t6)
    j .graph_floyd_warshall.20
.graph_floyd_warshall.28:
    lw a3,0(t6)
    slt a3,a3,t5
    mv s1,a3
    j .graph_floyd_warshall.30
.graph_floyd_warshall.30:
    mv a3,s1
    beq a3,zero, .graph_floyd_warshall.26
    j .graph_floyd_warshall.20
.graph_floyd_warshall.32:
    mv a0,a2
    mv a1,a3
    slt a2,a1,a4
    beq a2,zero, .graph_floyd_warshall.35
    j .graph_floyd_warshall.33
.graph_floyd_warshall.33:
    mv a2,a1
    li a6,4000
    mul a2,a2,a6
    add a2,a5,a2
    addi a3,a2,0
    mv t0,a0
    li t1,0
    j .graph_floyd_warshall.37
.graph_floyd_warshall.35:
    ld s0,52(sp)
    ld ra,60(sp)
    addi sp,sp,68
    ret
.graph_floyd_warshall.37:
    mv a0,t0
    mv a2,t1
    slt t0,a2,a4
    beq t0,zero, .graph_floyd_warshall.40
    j .graph_floyd_warshall.38
.graph_floyd_warshall.38:
    mv t0,a2
    li a6,4
    mul t0,t0,a6
    add t0,a3,t0
    lw t0,0(t0)
    add a0,a0,t0
    addi a2,a2,1
    mv t0,a0
    mv t1,a2
    j .graph_floyd_warshall.37
.graph_floyd_warshall.40:
    addi a1,a1,1
    mv a2,a0
    mv a3,a1
    j .graph_floyd_warshall.32
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
    li a0,1000
    li a1,10
    sd a2,-40(s0)
    call graph_floyd_warshall
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
    .globl graph_weight
    .section .data
graph_weight:
    .zero  4000000
    .globl .temp0
    .section .data
.temp0:
    .asciz  "%d"
