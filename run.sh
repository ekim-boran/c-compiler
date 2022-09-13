## test simplify_cfg 
#
##diff -r -B -Z  ./examples/simplify_cfg ./examples/simplify_cfg_out
#
##./run.sh ./examples/ir0 ./examples/ir_
##./run.sh ./examples/ir0 ./examples/ir0_out
##./run.sh ./examples/ir1 ./examples/ir1_out
##./run.sh ./examples/ir0 ./examples/final
#
##echo "results" > results.txt
##for f in $1/*
##do
##    filename=$(basename -- "$f")
##    extension="${filename##*.}"
##    filename="${filename%.*}"
##    echo $filename >> results.txt
##    ./kecc  --irrun $f | tail -1  | awk ' {print $2}' >> results.txt
##done
#
## ./run.sh ./examples/asm
#echo "results" > myresults.txt
#for f in $1/*
#do
#    filename=$(basename -- "$f")
#    extension="${filename##*.}"
#    filename="${filename%.*}"
#    if [ $extension = "ir" ] 
#    then
#        echo $filename >> myresults.txt
#        ./kecc  --irrun $f | tail -1  | awk  ' {print $2}' >> myresults.txt
#    else 
#       echo $filename >> myresults.txt
#       riscv64-linux-gnu-gcc -static  $f  -o ./examples/_tempexe/$filename >> /dev/null
#       qemu-riscv64-static ./examples/_tempexe/$filename 
#       echo $? >> myresults.txt
#    fi
#done
#cat myresults.txt
##diff -r -B -Z  results.txt myresults.txt
# 
#
##FILES="./examples/asmgen/*"
##for f in $FILES
##do
##    filename=$(basename -- "$f")
##    extension="${filename##*.}"
##    filename="${filename%.*}"
## 
##    echo "Processing $filename file..."
##    riscv64-linux-gnu-gcc -g -ggdb  -static  $f  -o ./examples/executable/$filename #
##    riscv64-linux-gnu-objdump -d  ./examples/executable/$filename > x.s
##    qemu-riscv64-static    ./examples/executable/$filename     #     -g 1201 
##    echo $? 
##done
##
#qemu-riscv64-static -g 1201  ./exec 
#
#
#
#clang -O0 -emit-llvm -c ./examples/bench/fibloop2.c #bc file
#
#clang -O1 -emit-llvm -c ./examples/bench/fibloop2.c #ll file
#
#clang -S -emit-llvm -O -Xclang -disable-llvm-passes ./examples/bench/fibloop2.c
#opt-13    fibloop2.ll  -mem2reg -gvn -licm -dce -o fibloop2-1.bc
#llvm-dis-13 fibloop2-1.bc -o fibloop2-1.ll