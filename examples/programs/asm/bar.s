    .globl bar
    .section .text
    .type bar @function
bar:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.bar.0:
    xor a0,a0,a1
    seqz a0,a0
    beq a0,zero, .bar.2
    j .bar.1
.bar.1:
    mv a0,a1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
.bar.2:
    mv a0,a2
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
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
