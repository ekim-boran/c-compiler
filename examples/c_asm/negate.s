    .globl foo
    .section .text
    .type foo @function
foo:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.foo.0:
    xor a0,a0,a1
    seqz a0,a0
    seqz a0,a0
    beq a0,zero, .foo.2
    j .foo.1
.foo.1:
    mv a0,a1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.foo.2:
    mv a0,a2
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-40
    sd ra,32(sp)
    sd s0,24(sp)
    addi s0,sp,40
.main.0:
    li a6,1
    negw a2,a6
    li a0,0
    li a1,1
    call foo
    li a6,1
    xor a0,a0,a6
    seqz a0,a0
    ld s0,24(sp)
    ld ra,32(sp)
    addi sp,sp,40
    ret
