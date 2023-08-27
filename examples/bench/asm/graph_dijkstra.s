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
    li a6,8000
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
    .globl graph_dijkstra
    .section .text
    .type graph_dijkstra @function
graph_dijkstra:
    addi sp,sp,-68
    sd ra,60(sp)
    sd s0,52(sp)
    addi s0,sp,68
.graph_dijkstra.0:
    mv a4,a0
    li a6,2000
    slt a0,a6,a4
    xori a0,a0,1
    seqz a0,a0
    beq a0,zero, .graph_dijkstra.2
    j .graph_dijkstra.1
.graph_dijkstra.1:
    mv a0,a1
    ld s0,52(sp)
    ld ra,60(sp)
    addi sp,sp,68
    ret
.graph_dijkstra.2:
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
    negw a1,a6
    li a2,0
    j .graph_dijkstra.5
.graph_dijkstra.3:
    mv t0,a2
    li a6,4
    mul a2,t0,a6
    add a3,t1,a2
    li a6,8000
    mul t0,t0,a6
    add t0,a5,t0
    addi t0,t0,0
    lw t2,0(a3)
    la a7,graph_dijkstra_visited
    addi t3,a7,0
    add a2,t3,a2
    li a6,1
    sw a6,0(a2)
    li a3,0
    j .graph_dijkstra.35
.graph_dijkstra.5:
    mv a0,a2
    slt a2,a0,a4
    beq a2,zero, .graph_dijkstra.8
    j .graph_dijkstra.6
.graph_dijkstra.6:
    la a7,graph_dijkstra_dist
    addi a2,a7,0
    mv a3,a0
    li a6,4
    mul a3,a3,a6
    add a2,a2,a3
    sw a1,0(a2)
    la a7,graph_dijkstra_visited
    addi a2,a7,0
    add a2,a2,a3
    li a6,0
    sw a6,0(a2)
    addi a0,a0,1
    mv a2,a0
    j .graph_dijkstra.5
.graph_dijkstra.8:
    la a7,graph_dijkstra_dist
    addi t1,a7,0
    addi a0,t1,0
    li a6,0
    sw a6,0(a0)
    li a2,0
    j .graph_dijkstra.10
.graph_dijkstra.10:
    mv a0,a2
    slt a2,a0,a4
    li a3,0
    li t3,0
    beq a2,zero, .graph_dijkstra.49
    mv t0,a1
    li t2,0
    j .graph_dijkstra.15
.graph_dijkstra.15:
    mv a2,t0
    mv a3,t2
    slt t0,a3,a4
    beq t0,zero, .graph_dijkstra.55
    j .graph_dijkstra.16
.graph_dijkstra.16:
    mv t0,a3
    li a6,4
    mul t0,t0,a6
    add t2,t1,t0
    lw t3,0(t2)
    xor t3,t3,a1
    snez t3,t3
    li t4,0
    beq t3,zero, .graph_dijkstra.24
    j .graph_dijkstra.22
.graph_dijkstra.17:
    mv a2,t3
    addi a3,a3,1
    mv t0,a2
    mv t2,a3
    j .graph_dijkstra.15
.graph_dijkstra.20:
    xor t0,a2,a1
    snez t0,t0
    li t3,0
    beq t0,zero, .graph_dijkstra.30
    j .graph_dijkstra.28
.graph_dijkstra.22:
    la a7,graph_dijkstra_visited
    addi t3,a7,0
    add t0,t3,t0
    lw t0,0(t0)
    seqz t0,t0
    li a6,0
    xor t0,t0,a6
    snez t0,t0
    mv t4,t0
    j .graph_dijkstra.24
.graph_dijkstra.24:
    mv t0,t4
    seqz t0,t0
    beq t0,zero, .graph_dijkstra.20
    mv t3,a2
    j .graph_dijkstra.17
.graph_dijkstra.28:
    mv t0,a2
    li a6,4
    mul t0,t0,a6
    add t0,t1,t0
    lw t0,0(t0)
    lw t2,0(t2)
    slt t0,t0,t2
    mv t3,t0
    j .graph_dijkstra.30
.graph_dijkstra.30:
    mv t0,t3
    mv t3,a3
    beq t0,zero, .graph_dijkstra.17
    mv t3,a2
    j .graph_dijkstra.17
.graph_dijkstra.31:
    beq a3,zero, .graph_dijkstra.3
    j .graph_dijkstra.31
.graph_dijkstra.35:
    mv a2,a3
    slt a3,a2,a4
    beq a3,zero, .graph_dijkstra.38
    j .graph_dijkstra.36
.graph_dijkstra.36:
    mv a3,a2
    li a6,4
    mul t4,a3,a6
    add a3,t3,t4
    lw a3,0(a3)
    li a6,0
    xor a3,a3,a6
    snez a3,a3
    beq a3,zero, .graph_dijkstra.40
    j .graph_dijkstra.37
.graph_dijkstra.37:
    addi a2,a2,1
    mv a3,a2
    j .graph_dijkstra.35
.graph_dijkstra.38:
    addi a0,a0,1
    mv a2,a0
    j .graph_dijkstra.10
.graph_dijkstra.40:
    add t5,t1,t4
    lw a3,0(t5)
    xor a3,a3,a1
    snez a3,a3
    li t6,0
    beq a3,zero, .graph_dijkstra.47
    j .graph_dijkstra.45
.graph_dijkstra.43:
    add a3,t0,t4
    lw a3,0(a3)
    add a3,t2,a3
    sw a3,0(t5)
    j .graph_dijkstra.37
.graph_dijkstra.45:
    lw a3,0(t5)
    add t6,t0,t4
    lw t6,0(t6)
    add t6,t2,t6
    slt a3,a3,t6
    mv t6,a3
    j .graph_dijkstra.47
.graph_dijkstra.47:
    mv a3,t6
    beq a3,zero, .graph_dijkstra.43
    j .graph_dijkstra.37
.graph_dijkstra.49:
    mv a0,a3
    mv a1,t3
    slt a2,a1,a4
    beq a2,zero, .graph_dijkstra.52
    j .graph_dijkstra.50
.graph_dijkstra.50:
    mv a2,a1
    li a6,4
    mul a2,a2,a6
    add a2,t1,a2
    lw a2,0(a2)
    add a0,a0,a2
    addi a1,a1,1
    mv a3,a0
    mv t3,a1
    j .graph_dijkstra.49
.graph_dijkstra.52:
    ld s0,52(sp)
    ld ra,60(sp)
    addi sp,sp,68
    ret
.graph_dijkstra.55:
    xor a3,a2,a1
    seqz a3,a3
    beq a3,zero, .graph_dijkstra.3
    j .graph_dijkstra.31
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
    li a0,2000
    li a1,10
    sd a2,-40(s0)
    call graph_dijkstra
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
    .globl graph_dijkstra_dist
    .section .data
graph_dijkstra_dist:
    .zero  8000
    .globl graph_dijkstra_visited
    .section .data
graph_dijkstra_visited:
    .zero  8000
    .globl graph_weight
    .section .data
graph_weight:
    .zero  16000000
    .globl .temp0
    .section .data
.temp0:
    .asciz  "%d"
