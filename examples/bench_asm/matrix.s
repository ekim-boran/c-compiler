    .globl matrix_init
    .section .text
    .type matrix_init @function
matrix_init:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.matrix_init.0:
    addi a5,a1,1
    li a4,0
    j .matrix_init.2
.matrix_init.2:
    mv a1,a4
    slt a4,a1,a0
    beq a4,zero, .matrix_init.5
    j .matrix_init.3
.matrix_init.3:
    mv a4,a1
    li a6,2000
    mul a4,a4,a6
    add a4,a3,a4
    addi t0,a4,0
    li t1,0
    j .matrix_init.7
.matrix_init.5:
    li a0,0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.matrix_init.7:
    mv a4,t1
    slt t1,a4,a0
    beq t1,zero, .matrix_init.10
    j .matrix_init.8
.matrix_init.8:
    mv t1,a4
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
    beq t1,zero, .matrix_init.13
    j .matrix_init.11
.matrix_init.10:
    addi a1,a1,1
    mv a4,a1
    j .matrix_init.2
.matrix_init.11:
    lw t1,0(a2)
    addi t1,t1,1
    sw t1,0(a2)
    j .matrix_init.13
.matrix_init.13:
    addi a4,a4,1
    mv t1,a4
    j .matrix_init.7
    .globl matrix_mul
    .section .text
    .type matrix_mul @function
matrix_mul:
    addi sp,sp,-84
    sd ra,76(sp)
    sd s0,68(sp)
    addi s0,sp,84
.matrix_mul.0:
    mv a5,a0
    mv t0,a1
    li a6,500
    slt a0,a6,a5
    xori a0,a0,1
    seqz a0,a0
    beq a0,zero, .matrix_mul.2
    j .matrix_mul.1
.matrix_mul.1:
    mv a0,t0
    ld s0,68(sp)
    ld ra,76(sp)
    addi sp,sp,84
    ret
.matrix_mul.2:
    addi a7,s0,-20
    li a6,0
    sw a6,0(a7)
    la a7,matrix_a
    addi t1,a7,0
    mv a0,a5
    mv a1,t0
    addi a7,s0,-20
    mv a2,a7
    mv a3,t1
    sd a4,-84(s0)
    sw a5,-76(s0)
    sw t0,-68(s0)
    sd t1,-60(s0)
    call matrix_init
    ld a4,-84(s0)
    lw a5,-76(s0)
    lw t0,-68(s0)
    ld t1,-60(s0)
    la a7,matrix_b
    addi t2,a7,0
    mv a0,a5
    mv a1,t0
    addi a7,s0,-20
    mv a2,a7
    mv a3,t2
    sw a5,-84(s0)
    sd t1,-76(s0)
    sd t2,-68(s0)
    call matrix_init
    lw a5,-84(s0)
    ld t1,-76(s0)
    ld t2,-68(s0)
    li a2,0
    li a3,0
    j .matrix_mul.5
.matrix_mul.5:
    mv a0,a2
    mv a1,a3
    slt a2,a1,a5
    beq a2,zero, .matrix_mul.8
    j .matrix_mul.6
.matrix_mul.6:
    mv a2,a1
    li a6,2000
    mul a2,a2,a6
    add a3,t1,a2
    addi a3,a3,0
    mv a4,a0
    li t0,0
    j .matrix_mul.10
.matrix_mul.8:
    ld s0,68(sp)
    ld ra,76(sp)
    addi sp,sp,84
    ret
.matrix_mul.10:
    mv a0,a4
    mv a4,t0
    slt t0,a4,a5
    beq t0,zero, .matrix_mul.13
    j .matrix_mul.11
.matrix_mul.11:
    la a7,matrix_c
    addi t0,a7,0
    add t0,t0,a2
    addi t0,t0,0
    mv t3,a4
    li a6,4
    mul t3,t3,a6
    add t4,t0,t3
    li a6,0
    sw a6,0(t4)
    li t5,0
    j .matrix_mul.15
.matrix_mul.13:
    addi a1,a1,1
    mv a2,a0
    mv a3,a1
    j .matrix_mul.5
.matrix_mul.15:
    mv t0,t5
    slt t5,t0,a5
    beq t5,zero, .matrix_mul.18
    j .matrix_mul.16
.matrix_mul.16:
    lw t5,0(t4)
    mv t6,t0
    li a6,4
    mul s1,t6,a6
    add s1,a3,s1
    lw s1,0(s1)
    li a6,2000
    mul t6,t6,a6
    add t6,t2,t6
    addi t6,t6,0
    add t6,t6,t3
    lw t6,0(t6)
    mul t6,s1,t6
    add t5,t5,t6
    sw t5,0(t4)
    addi t0,t0,1
    mv t5,t0
    j .matrix_mul.15
.matrix_mul.18:
    lw t0,0(t4)
    add a0,a0,t0
    addi t3,a4,1
    mv a4,a0
    mv t0,t3
    j .matrix_mul.10
    .globl matrix_add
    .section .text
    .type matrix_add @function
matrix_add:
    addi sp,sp,-84
    sd ra,76(sp)
    sd s0,68(sp)
    addi s0,sp,84
.matrix_add.0:
    mv a5,a0
    mv t0,a1
    li a6,500
    slt a0,a6,a5
    xori a0,a0,1
    seqz a0,a0
    beq a0,zero, .matrix_add.2
    j .matrix_add.1
.matrix_add.1:
    mv a0,t0
    ld s0,68(sp)
    ld ra,76(sp)
    addi sp,sp,84
    ret
.matrix_add.2:
    addi a7,s0,-20
    li a6,0
    sw a6,0(a7)
    la a7,matrix_a
    addi t1,a7,0
    mv a0,a5
    mv a1,t0
    addi a7,s0,-20
    mv a2,a7
    mv a3,t1
    sd a4,-84(s0)
    sw a5,-76(s0)
    sw t0,-68(s0)
    sd t1,-60(s0)
    call matrix_init
    ld a4,-84(s0)
    lw a5,-76(s0)
    lw t0,-68(s0)
    ld t1,-60(s0)
    la a7,matrix_b
    addi t2,a7,0
    mv a0,a5
    mv a1,t0
    addi a7,s0,-20
    mv a2,a7
    mv a3,t2
    sw a5,-84(s0)
    sw t0,-76(s0)
    sd t1,-68(s0)
    sd t2,-60(s0)
    call matrix_init
    lw a5,-84(s0)
    lw t0,-76(s0)
    ld t1,-68(s0)
    ld t2,-60(s0)
    li a2,0
    li a3,0
    j .matrix_add.5
.matrix_add.5:
    mv a0,a2
    mv a1,a3
    slt a2,a1,a5
    beq a2,zero, .matrix_add.8
    j .matrix_add.6
.matrix_add.6:
    mv a2,a1
    li a6,2000
    mul a2,a2,a6
    add a3,t1,a2
    addi a3,a3,0
    add a4,t2,a2
    addi a4,a4,0
    mv t3,a0
    li t4,0
    j .matrix_add.10
.matrix_add.8:
    ld s0,68(sp)
    ld ra,76(sp)
    addi sp,sp,84
    ret
.matrix_add.10:
    mv a0,t3
    mv t3,t4
    slt t4,t3,a5
    beq t4,zero, .matrix_add.13
    j .matrix_add.11
.matrix_add.11:
    la a7,matrix_c
    addi t4,a7,0
    add t4,t4,a2
    addi t4,t4,0
    mv t5,t3
    li a6,4
    mul t5,t5,a6
    add t4,t4,t5
    add t6,a3,t5
    lw t6,0(t6)
    add t5,a4,t5
    lw t5,0(t5)
    mul t5,t0,t5
    add t5,t6,t5
    sw t5,0(t4)
    lw t4,0(t4)
    add a0,a0,t4
    addi t5,t3,1
    mv t3,a0
    mv t4,t5
    j .matrix_add.10
.matrix_add.13:
    addi a1,a1,1
    mv a2,a0
    mv a3,a1
    j .matrix_add.5
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-48
    sd ra,40(sp)
    sd s0,32(sp)
    addi s0,sp,48
.main.0:
    call clock
    mv a2,a0
    li a0,500
    li a1,11
    sd a2,-48(s0)
    call matrix_mul
    ld a2,-48(s0)
    mv a3,a0
    li a0,500
    li a1,12
    sd a2,-48(s0)
    sw a3,-40(s0)
    call matrix_add
    ld a2,-48(s0)
    lw a3,-40(s0)
    add a3,a3,a0
    sd a2,-48(s0)
    sw a3,-40(s0)
    call clock
    ld a2,-48(s0)
    lw a3,-40(s0)
    sub a1,a0,a2
    la a7,.temp0
    mv a0,a7
    sw a3,-48(s0)
    call printf
    lw a3,-48(s0)
    mv a0,a3
    ld s0,32(sp)
    ld ra,40(sp)
    addi sp,sp,48
    ret
    .globl matrix_a
    .section .data
matrix_a:
    .zero  1000000
    .globl matrix_b
    .section .data
matrix_b:
    .zero  1000000
    .globl matrix_c
    .section .data
matrix_c:
    .zero  1000000
    .globl .temp0
    .section .data
.temp0:
    .asciz  "%d"
