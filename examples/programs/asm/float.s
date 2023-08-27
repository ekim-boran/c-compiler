    .globl custom_abs
    .section .text
    .type custom_abs @function
custom_abs:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.custom_abs.0:
    li a6,0
    fcvt.d.w fa1,a6
    flt.d a0,fa0,fa1
    fmv.d fa1,fa0
    beq a0,zero, .custom_abs.3
    j .custom_abs.1
.custom_abs.1:
    fneg.d fa0,fa0
    fmv.d fa1,fa0
    j .custom_abs.3
.custom_abs.3:
    fmv.d fa0,fa1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl custom_max
    .section .text
    .type custom_max @function
custom_max:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.custom_max.0:
    flt.d a0,fa1,fa0
    fmv.d fa2,fa1
    beq a0,zero, .custom_max.3
    fmv.d fa2,fa0
    j .custom_max.3
.custom_max.3:
    fmv.d fa0,fa2
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl is_close
    .section .text
    .type is_close @function
is_close:
    addi sp,sp,-56
    sd ra,48(sp)
    sd s0,40(sp)
    addi s0,sp,56
.is_close.0:
    fmv.d fa4,fa0
    fsub.d fa0,fa4,fa1
    fsd fa4,-56(s0)
    fsd fa1,-48(s0)
    fsd fa2,-40(s0)
    fsd fa3,-32(s0)
    call custom_abs
    fld fa4,-56(s0)
    fld fa1,-48(s0)
    fld fa2,-40(s0)
    fld fa3,-32(s0)
    fmv.d fa5,fa0
    fmv.d fa0,fa4
    fsd fa1,-56(s0)
    fsd fa2,-48(s0)
    fsd fa3,-40(s0)
    fsd fa5,-32(s0)
    call custom_abs
    fld fa1,-56(s0)
    fld fa2,-48(s0)
    fld fa3,-40(s0)
    fld fa5,-32(s0)
    fmv.d fa4,fa0
    fmv.d fa0,fa1
    fsd fa2,-56(s0)
    fsd fa3,-48(s0)
    fsd fa5,-40(s0)
    fsd fa4,-32(s0)
    call custom_abs
    fld fa2,-56(s0)
    fld fa3,-48(s0)
    fld fa5,-40(s0)
    fld fa4,-32(s0)
    fmv.d fa1,fa0
    fmv.d fa0,fa4
    fsd fa2,-56(s0)
    fsd fa3,-48(s0)
    fsd fa5,-40(s0)
    call custom_max
    fld fa2,-56(s0)
    fld fa3,-48(s0)
    fld fa5,-40(s0)
    fmul.d fa0,fa2,fa0
    fmv.d fa1,fa3
    fsd fa5,-56(s0)
    call custom_max
    fld fa5,-56(s0)
    flt.d a0,fa0,fa5
    xori a0,a0,1
    ld s0,40(sp)
    ld ra,48(sp)
    addi sp,sp,56
    ret
    .globl average
    .section .text
    .type average @function
average:
    addi sp,sp,-16
    sd ra,8(sp)
    sd s0,0(sp)
    addi s0,sp,16
.average.0:
    li a4,0
    li a5,0
    j .average.2
.average.2:
    mv a2,a4
    mv a3,a5
    slt a4,a3,a0
    beq a4,zero, .average.5
    j .average.3
.average.3:
    mv a4,a3
    li a6,4
    mul a4,a4,a6
    add a4,a1,a4
    lw a4,0(a4)
    add a2,a2,a4
    addi a3,a3,1
    mv a4,a2
    mv a5,a3
    j .average.2
.average.5:
    fcvt.d.w fa0,a2
    fcvt.d.w fa1,a0
    fdiv.d fa0,fa0,fa1
    ld s0,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret
    .globl main
    .section .text
    .type main @function
main:
    addi sp,sp,-88
    sd ra,80(sp)
    sd s0,72(sp)
    addi s0,sp,88
.main.0:
    li a2,0
    j .main.2
.main.2:
    mv a1,a2
    li a6,10
    slt a2,a1,a6
    beq a2,zero, .main.5
    j .main.3
.main.3:
    addi a7,s0,-56
    addi a2,a7,0
    mv a3,a1
    li a6,4
    mul a3,a3,a6
    add a2,a2,a3
    sw a1,0(a2)
    addi a1,a1,1
    mv a2,a1
    j .main.2
.main.5:
    addi a7,s0,-56
    addi a1,a7,0
    li a0,10
    call average
    fcvt.s.d fa0,fa0
    fcvt.d.s fa0,fa0
    la a7,.temp0
    fld fa7,0(a7)
    fmv.d fa1,fa7
    la a7,.temp1
    fld fa7,0(a7)
    fmv.d fa2,fa7
    la a7,.temp2
    fld fa7,0(a7)
    fmv.d fa3,fa7
    call is_close
    ld s0,72(sp)
    ld ra,80(sp)
    addi sp,sp,88
    ret
    .globl .temp2
    .section .data
.temp2:
    .quad  0x3fb999999999999a
    .globl .temp1
    .section .data
.temp1:
    .quad  0x3e112e0be826d695
    .globl .temp0
    .section .data
.temp0:
    .quad  0x4012000000000000
