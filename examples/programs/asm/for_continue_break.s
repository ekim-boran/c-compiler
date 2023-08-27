    .globl foo
    .section .text
    .type foo @function
foo:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.foo.0:
    li a2,0
    li a3,0
    j .foo.2
.foo.2:
    mv a0,a2
    mv a1,a3
    li a6,5
    xor a2,a1,a6
    seqz a2,a2
    beq a2,zero, .foo.7
    j .foo.6
.foo.4:
    mv a0,a2
    mv a1,a3
    mv a2,a0
    mv a3,a1
    j .foo.2
.foo.6:
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.foo.7:
    li a6,3
    xor a2,a1,a6
    seqz a2,a2
    beq a2,zero, .foo.10
    j .foo.9
.foo.9:
    addi a1,a1,1
    mv a2,a0
    mv a3,a1
    j .foo.4
.foo.10:
    add a0,a0,a1
    addi a1,a1,1
    mv a2,a0
    mv a3,a1
    j .foo.4
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
    li a6,7
    xor a0,a0,a6
    seqz a0,a0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
