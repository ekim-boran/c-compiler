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
    addi sp,sp,-48
    sd ra,40(sp)
    sd s0,32(sp)
    addi s0,sp,48
.main.0:
    li a6,1
    negw a3,a6
    li a0,0
    li a1,1
    mv a2,a3
    sw a3,-48(s0)
    call foo
    lw a3,-48(s0)
    xor a0,a0,a3
    seqz a0,a0
    ld s0,32(sp)
    ld ra,40(sp)
    addi sp,sp,48
    ret
