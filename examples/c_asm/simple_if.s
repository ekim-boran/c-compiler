    .globl fibonacci
    .section .text
    .type fibonacci @function
fibonacci:
    addi sp,sp,-32
    sd ra,24(sp)
    sd s0,16(sp)
    addi s0,sp,32
.fibonacci.0:
    li a6,2
    slt a1,a0,a6
    mv a2,a0
    beq a1,zero, .fibonacci.3
    j .fibonacci.1
.fibonacci.1:
    addi a0,a0,2
    mv a2,a0
    j .fibonacci.3
.fibonacci.3:
    mv a1,a2
    li a6,2
    sub a0,a1,a6
    sw a1,-32(s0)
    call fibonacci
    lw a1,-32(s0)
    mv a2,a0
    li a6,1
    sub a0,a1,a6
    sw a2,-32(s0)
    call fibonacci
    lw a2,-32(s0)
    add a0,a2,a0
    ld s0,16(sp)
    ld ra,24(sp)
    addi sp,sp,32
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    li a0,1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
