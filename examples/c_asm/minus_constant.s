    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.main.0:
    la a7,a
    lw a0,0(a7)
    la a7,b
    ld a1,0(a7)
    add a0,a0,a1
    la a7,c
    flw fa0,0(a7)
    fcvt.w.s a1,fa0 ,rtz
    add a0,a0,a1
    la a7,d
    fld fa0,0(a7)
    fcvt.l.d a1,fa0 ,rtz
    add a0,a0,a1
    li a6,4
    negw a1,a6
    xor a0,a0,a1
    seqz a0,a0
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl a
    .section .data
a:  .word  0xffffffff
    .globl b
    .section .data
b:  .quad  0xffffffffffffffff
    .globl c
    .section .data
c:  .word  0xbfc00000
    .globl d
    .section .data
d:  .quad  0xbff8000000000000
