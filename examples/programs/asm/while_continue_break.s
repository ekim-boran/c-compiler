    .globl foo
    .section .text
    .type foo @function
foo:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.foo.0:
    la a7,nonce
    lw a0,0(a7)
    li a6,98
    rem a2,a0,a6
    addi a3,a2,2
    li a4,0
    li a5,0
    j .foo.2
.foo.2:
    mv a0,a4
    mv a1,a5
    li a6,100
    slt a4,a1,a6
    mv a5,a0
    beq a4,zero, .foo.5
    j .foo.3
.foo.3:
    xor a4,a1,a2
    seqz a4,a4
    beq a4,zero, .foo.7
    j .foo.6
.foo.4:
    mv a0,t0
    mv a1,t1
    mv a4,a0
    mv a5,a1
    j .foo.2
.foo.5:
    mv a0,a5
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.foo.6:
    addi a1,a1,1
    mv t0,a0
    mv t1,a1
    j .foo.4
.foo.7:
    add a0,a0,a1
    addi a1,a1,1
    xor a4,a1,a3
    seqz a4,a4
    mv t0,a0
    mv t1,a1
    beq a4,zero, .foo.4
    mv a5,a0
    j .foo.5
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    call foo
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl nonce
    .section .data
nonce:
    .word  0xc
