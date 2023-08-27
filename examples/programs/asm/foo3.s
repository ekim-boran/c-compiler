    .globl foo
    .section .text
    .type foo @function
foo:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.foo.0:
    add a0,a0,a1
    la a7,nonce
    lw a1,0(a7)
    add a0,a0,a1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-32
    sd ra,24(sp)
    sd s0,16(sp)
    addi s0,sp,32
.main.0:
    la a7,g
    lw a1,0(a7)
    mv a0,a1
    call foo
    ld s0,16(sp)
    ld ra,24(sp)
    addi sp,sp,32
    ret
    .globl nonce
    .section .data
nonce:
    .word  0xc
    .globl g
    .section .data
g:  .word  0xa
