set confirm off
set architecture riscv:rv64
target remote 127.0.0.1:1201
set disassemble-next-line auto
tui enable
layout asm
layout regs