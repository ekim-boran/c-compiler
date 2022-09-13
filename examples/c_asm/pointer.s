    .globl foo
    .section .text
    .type foo @function
foo:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.foo.0:
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-44
    sd ra,36(sp)
    sd s0,28(sp)
    addi s0,sp,44
.main.0:
    addi a7,s0,-20
    li a6,1
    sw a6,0(a7)
    addi a7,s0,-20
    mv a0,a7
    sd a1,-44(s0)
    call foo
    ld a1,-44(s0)
    mv a2,a0
    addi a7,s0,-20
    mv a0,a7
    sd a1,-44(s0)
    sd a2,-36(s0)
    call foo
    ld a1,-44(s0)
    ld a2,-36(s0)
    lw a0,0(a0)
    addi a0,a0,1
    sw a0,0(a2)
    addi a7,s0,-20
    mv a0,a7
    sd a1,-44(s0)
    call foo
    ld a1,-44(s0)
    mv a2,a0
    addi a7,s0,-20
    mv a0,a7
    sd a1,-44(s0)
    sd a2,-36(s0)
    call foo
    ld a1,-44(s0)
    ld a2,-36(s0)
    lw a0,0(a0)
    addi a0,a0,1
    sw a0,0(a2)
    addi a7,s0,-20
    lw a0,0(a7)
    li a6,3
    xor a0,a0,a6
    seqz a0,a0
    ld s0,28(sp)
    ld ra,36(sp)
    addi sp,sp,44
    ret
