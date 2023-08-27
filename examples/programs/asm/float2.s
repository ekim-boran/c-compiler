    .globl func_67
    .section .text
    .type func_67 @function
func_67:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.func_67.0:
    la a7,a
    fld fa0,0(a7)
    la a7,b
    fld fa1,0(a7)
    fadd.d fa0,fa0,fa1
    la a7,c
    fld fa1,0(a7)
    fadd.d fa0,fa0,fa1
    la a7,d
    fld fa1,0(a7)
    fadd.d fa0,fa0,fa1
    fcvt.s.d fa0,fa0
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
    call func_67
    li a6,1000
    fcvt.s.w fa1,a6
    fmul.s fa0,fa0,fa1
    fcvt.w.s a0,fa0 ,rtz
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl a
    .section .data
a:  .quad  0x3ff9fb3080000000
    .globl b
    .section .data
b:  .quad  0x3fef3675e0000000
    .globl c
    .section .data
c:  .quad  0x3fe5f74e00000000
    .globl d
    .section .data
d:  .quad  0x3fda15c100000000
