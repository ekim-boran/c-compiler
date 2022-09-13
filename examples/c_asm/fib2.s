    .globl fibonacci
    .section .text
    .type fibonacci @function
fibonacci:
    addi sp,sp,-32
    sd ra,24(sp)
    sd s0,16(sp)
    addi s0,sp,32
.fibonacci.0:
    mv a1,a0
    li a6,2
    slt a0,a1,a6
    beq a0,zero, .fibonacci.2
    j .fibonacci.1
.fibonacci.1:
    mv a0,a1
    ld s0,16(sp)
    ld ra,24(sp)
    addi sp,sp,32
    ret
.fibonacci.2:
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
    addi sp,sp,-24
    sd ra,16(sp)
    sd s0,8(sp)
    addi s0,sp,24
.main.0:
    li a0,9
    call fibonacci
    li a6,34
    xor a0,a0,a6
    seqz a0,a0
    ld s0,8(sp)
    ld ra,16(sp)
    addi sp,sp,24
    ret
