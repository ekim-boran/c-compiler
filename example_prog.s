    .globl exists
    .section .text
    .type exists @function
exists:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.exists.0:
    mv a2,a0
    j .exists.2
.exists.2:
    mv a0,a2
    addi a2,a0,8
    ld a3,0(a2)
    li a6,0
    xor a3,a3,a6
    snez a3,a3
    beq a3,zero, .exists.5
    j .exists.3
.exists.3:
    addi a0,a0,0
    lw a3,0(a0)
    xor a3,a3,a1
    seqz a3,a3
    beq a3,zero, .exists.7
    j .exists.6
.exists.5:
    li a0,0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.exists.6:
    li a0,1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.exists.7:
    lw a0,0(a0)
    slt a0,a1,a0
    beq a0,zero, .exists.10
    j .exists.9
.exists.9:
    li a0,0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.exists.10:
    ld a0,0(a2)
    mv a2,a0
    j .exists.2
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-56
    sd ra,48(sp)
    sd s0,40(sp)
    addi s0,sp,56
.main.0:
    la a7,.temp0
    mv a0,a7
    li a1,0
    call printf
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
    li a6,2
    mul a1,a2,a6
    sw a1,0(a0)
    ld a0,0(a4)
    addi a1,a2,1
    mv a4,a0
    mv a2,a1
    j .main.2
.main.7:
    mv a2,a4
    mv a0,a5
    addi a1,a0,8
    ld a4,0(a1)
    li a6,0
    xor a4,a4,a6
    snez a4,a4
    beq a4,zero, .main.10
    j .main.8
.main.8:
    addi a0,a0,0
    lw a0,0(a0)
    add a0,a2,a0
    ld a1,0(a1)
    mv a4,a0
    mv a5,a1
    j .main.7
.main.10:
    la a7,.temp1
    mv a0,a7
    mv a1,a2
    sw a2,-56(s0)
    sd a3,-48(s0)
    call printf
    lw a2,-56(s0)
    ld a3,-48(s0)
    mv a0,a3
    li a1,1022
    sw a2,-56(s0)
    call exists
    lw a2,-56(s0)
    mv a1,a0
    la a7,.temp2
    mv a0,a7
    sw a2,-56(s0)
    call printf
    lw a2,-56(s0)
    mv a0,a2
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
    .globl .temp2
    .section .data
.temp2:
    .asciz  "1022 exists %d in list
"
    .globl .temp1
    .section .data
.temp1:
    .asciz  "sum is %d
"
    .globl .temp0
    .section .data
.temp0:
    .asciz  "Hello world
"
