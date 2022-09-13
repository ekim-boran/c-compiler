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
    addi sp,sp,-80
    sd ra,72(sp)
    sd s0,64(sp)
    addi s0,sp,80
.main.0:
    li a2,0
    j .main.2
.main.2:
    li a6,10
    slt a0,a2,a6
    beq a0,zero, .main.5
    j .main.3
.main.3:
    addi a7,s0,-56
    addi a0,a7,0
    sd a1,-80(s0)
    sw a2,-72(s0)
    call foo
    ld a1,-80(s0)
    lw a2,-72(s0)
    mv a3,a2
    li a6,4
    mul a3,a3,a6
    add a0,a0,a3
    sw a2,0(a0)
    addi a0,a2,1
    mv a2,a0
    j .main.2
.main.5:
    addi a7,s0,-56
    addi a0,a7,0
    addi a0,a0,20
    lw a0,0(a0)
    li a6,5
    xor a0,a0,a6
    seqz a0,a0
    ld s0,64(sp)
    ld ra,72(sp)
    addi sp,sp,80
    ret
