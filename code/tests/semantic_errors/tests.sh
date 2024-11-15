#!/bin/bash
set -e
#export LLVM_INSTALL_PATH=/home/gihan/LLVM/install-10.0.1
#export LLVM_INSTALL_PATH=/modules/cs325/llvm-8/
#export LLVM_INSTALL_PATH=/modules/cs325/llvm-10.0.1
#export LLVM_INSTALL_PATH=/modules/cs325/llvm-12.0.1
#export LLVM_INSTALL_PATH=/tmp/LLVM/llvm-14.0.6
#export LLVM_INSTALL_PATH=/modules/cs325/llvm-15.0.0
#export LLVM_INSTALL_PATH=/modules/cs325/llvm-16.0.0
#export LLVM_INSTALL_PATH=/modules/cs325/llvm-17.0.1
export LLVM_INSTALL_PATH=/modules/cs325/llvm-18.1.8
export PATH=$LLVM_INSTALL_PATH/bin:$PATH
export LD_LIBRARY_PATH=$LLVM_INSTALL_PATH/lib:$LD_LIBRARY_PATH
CLANG=$LLVM_INSTALL_PATH/bin/clang++

module load GCC/13.3.0

DIR="$(pwd)"

### Build mccomp compiler
echo "Cleanup *****"
rm -rf ./mccomp

echo "Compile *****"

make clean
make -j mccomp

COMP=$DIR/mccomp
echo $COMP


echo "Test *****"

semantic_error01=1
semantic_error02=1
semantic_error03=1


cd tests/semantic_errors/semantic_error01

if [ $semantic_error01 == 1 ]
then	
	cd ../semantic_error01/
	pwd
	rm -rf output.ll semantic_error01
	"$COMP" ./semantic_error01.c
	echo "this should fail as function with said arguments does not exist"

fi

if [ $semantic_error02 == 1 ]
then	
	cd ../semantic_error02/
	pwd
	rm -rf output.ll semantic_error02
	"$COMP" ./semantic_error02.c
	echo "this should fail as function doesn't return"

fi

if [ $semantic_error03 == 1 ]
then	
	cd ../semantic_error03/
	pwd
	rm -rf output.ll semantic_error03
	"$COMP" ./semantic_error03.c
	echo "function param is uninitialised and is used in addition so bad!"

fi
