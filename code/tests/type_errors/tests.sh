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

type_mismatch01=1
type_mismatch02=1
type_mismatch03=1
type_mismatch04=1
type_mismatch05=1
type_mismatch06=1
type_mismatch07=1
type_mismatch08=1
type_mismatch09=1
type_mismatch10=1

cd tests/type_errors/type_mismatch01

if [ $type_mismatch01 == 1 ]
then	
	cd ../type_mismatch01/
	pwd
	rm -rf output.ll type_mismatch01
	"$COMP" ./type_mismatch01.c
	echo "This should fail as we cannot narrow (f+y) to an int."

fi

if [ $type_mismatch02 == 1 ]
then	
	cd ../type_mismatch02/
	pwd
	rm -rf output.ll type_mismatch02
	"$COMP" ./type_mismatch02.c
	echo "This should fail as the function is bool and we cannot cast float to bool"

fi

if [ $type_mismatch03 == 1 ]
then	
	cd ../type_mismatch03/
	pwd
	rm -rf output.ll type_mismatch03
	"$COMP" ./type_mismatch03.c
	echo "Should succeed"
fi

if [ $type_mismatch04 == 1 ]
then	
	cd ../type_mismatch04/
	pwd
	rm -rf output.ll type_mismatch04
	"$COMP" ./type_mismatch04.c
	echo "Should fail as float cannot narrow to int"
fi

if [ $type_mismatch05 == 1 ]
then	
	cd ../type_mismatch05/
	pwd
	rm -rf output.ll type_mismatch05
	"$COMP" ./type_mismatch05.c
	echo "Should succeed"
fi

if [ $type_mismatch06 == 1 ]
then	
	cd ../type_mismatch06/
	pwd
	rm -rf output.ll type_mismatch06
	"$COMP" ./type_mismatch06.c
	echo "Should succeed "
fi

if [ $type_mismatch07 == 1 ]
then	
	cd ../type_mismatch07/
	pwd
	rm -rf output.ll type_mismatch07
	"$COMP" ./type_mismatch07.c
	echo "Should succeed "
fi

if [ $type_mismatch08 == 1 ]
then	
	cd ../type_mismatch08/
	pwd
	rm -rf output.ll type_mismatch08
	"$COMP" ./type_mismatch08.c
	echo "Should fail as can't cast x to bool "
fi

if [ $type_mismatch09 == 1 ]
then	
	cd ../type_mismatch09/
	pwd
	rm -rf output.ll type_mismatch09
	"$COMP" ./type_mismatch09.c
	echo "Should fail as void function can't return non-void"
fi

if [ $type_mismatch10 == 1 ]
then	
	cd ../type_mismatch10/
	pwd
	rm -rf output.ll type_mismatch10
	"$COMP" ./type_mismatch10.c
	echo "Should succeed"
fi
