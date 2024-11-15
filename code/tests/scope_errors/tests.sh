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

scope_error01=1
scope_error02=1
scope_error03=1
scope_error04=1
scope_error05=1
scope_error06=1
scope_error07=1



cd tests/scope_errors/scope_error01

if [ $scope_error01 == 1 ]
then	
	cd ../scope_error01/
	pwd
	rm -rf output.ll scope_error01
	"$COMP" ./scope_error01.c
	echo "Should succeed"

fi

if [ $scope_error02 == 1 ]
then	
	cd ../scope_error02/
	pwd
	rm -rf output.ll scope_error02
	"$COMP" ./scope_error02.c
	echo "Should fail as y is not defined"

fi

if [ $scope_error03 == 1 ]
then	
	cd ../scope_error03/
	pwd
	rm -rf output.ll scope_error03
	"$COMP" ./scope_error03.c
	echo "Should fail as function is not defined"
fi

if [ $scope_error04 == 1 ]
then	
	cd ../scope_error04/
	pwd
	rm -rf output.ll scope_error04
	"$COMP" ./scope_error04.c
	echo "Should fail as y is not defined"
fi

if [ $scope_error05 == 1 ]
then	
	cd ../scope_error05/
	pwd
	rm -rf output.ll scope_error05
	"$COMP" ./scope_error05.c
	echo "Should fail as y is not defined in this scope"
fi

if [ $scope_error06 == 1 ]
then	
	cd ../scope_error06/
	pwd
	rm -rf output.ll scope_error06
	"$COMP" ./scope_error06.c
	echo "Should fail as y is not defined"
fi

if [ $scope_error07 == 1 ]
then	
	cd ../scope_error07/
	pwd
	rm -rf output.ll scope_error07
	"$COMP" ./scope_error07.c
	echo "Should succeed"
fi


