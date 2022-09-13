
#PARAMS=(
#    -aa
#    -adce
#    -alignment-from-assumptions
#    -always-inline
#    -annotation2metadata
#    -annotation-remarks
#    -basic-aa
#    -bdce
#    -block-freq
#    -branch-prob
#    -called-value-propagation
#    -cg-profile
#    -deadargelim
#    -demanded-bits
#    -div-rem-pairs
#    -domtree
#    -early-cse
#    -early-cse-memssa
#    -float2int
#    -forceattrs
#    -function-attrs
#    -globaldce
#    -globalopt
#    -globals-aa
#    -indvars
#    -inferattrs
#    -inject-tli-mappings
#    -instcombine
#    -instsimplify
#    -ipsccp
#    -lcssa
#    -libcalls-shrinkwrap
#    -licm
#    -loop-deletion
#    -loop-distribute
#    -loop-idiom
#    -loop-load-elim
#    -loop-rotate
#    -loops
#    -loop-simplify
#    -loop-sink
#    -loop-unroll
#    -loop-vectorize
#    -lower-constant-intrinsics
#    -lower-expect
#    -mem2reg
#    -memcpyopt
#    -memoryssa
#    -pgo-memop-opt
#    -postdomtree
#    -reassociate
#    -rpo-function-attrs
#    -scalar-evolution
#    -sccp
#    -scoped-noalias-aa
#    -simplifycfg
#    -sroa
#    -strip-dead-prototypes
#    -targetlibinfo
#    -tbaa
#    -transform-warning
#    -vector-combine
#    -verify
#)
# 
#
#clang  -S  -emit-llvm -O -Xclang -disable-llvm-passes ./examples/bench/fibloop2.c
#opt-13    fibloop2.ll  -mem2reg -gvn -licm -dce -simplifycfg      -o fibloop2-1.bc
#
#opt-13    fibloop2.ll  -mem2reg -gvn -licm -dce -simplifycfg   ${PARAMS[@]}    -o fibloop2-2.bc
#llvm-dis-13 fibloop2-1.bc -o fibloop2-1.ll
#llvm-dis-13 fibloop2-2.bc -o fibloop2-2.ll
#
##llc-13   -relocation-model=pic -filetype=obj fibloop2-1.ll -o fibloop2.o
##gcc   fibloop2.o
##./a.out
#
# 
##clang-13 --target=riscv64 -march=rv64gc ./examples/bench/fibloop2.c -c -o x.o
##opt-13  -O1 -enable-new-pm=0 fibloop2.ll   -debug-pass=Arguments     -o fibloop2-1.bc
#
#