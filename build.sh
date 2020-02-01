#! /usr/bin/env bash
echo "==================================="
echo " Project Euler with Modern Fortran"
echo "==================================="
echo ""
./clean_build.sh
build_dir="`pwd`"

DATA_PATH="`pwd`/dat"

FCOMPILER="gfortran"
echo "Fortran compiler used: $FCOMPILER."

DEBUG_FLAG="-g -Wall -Wextra -Warray-temporaries -Wconversion \
    -fimplicit-none -fbacktrace -fcheck=all -finit-real=nan"
OPTIMIZE_FLAG="-O3"

srccodes=./src/*/*.f90
if [[ $1 = "--debug" ]]
then 
    COMPILE_FLAG="$DEBUG_FLAG"
elif [[ $1 = "--optimize" ]]
then 
    COMPILE_FLAG="$OPTIMIZE_FLAG"
else 
    COMPILE_FLAG="$OPTIMIZE_FLAG"
fi

if [ ! -d "./src" ]
then
    echo "ERROR: Failed to locate source file. Exit build."
    exit
else
    SRC="./src"
fi

if [ ! -d "./mod" ] || [ ! -d "./obj" ] || [ ! -d "./bin" ]
then
    mkdir mod obj bin
    MOD="`pwd`/mod" && OBJ="`pwd`/obj" && BIN="`pwd`/bin"
else
    echo "ERROR: Build files already exist. Exit build."
    exit
fi

echo "Generating interfaces ..."
cd $build_dir
$FCOMPILER $COMPILE_FLAG -J $MOD -c \
	$SRC/auto_gen/euler_file_generator_m.f90 \
    -o $OBJ/euler_file_generator_m.o
$FCOMPILER $COMPILE_FLAG -J $MOD -c \
	$SRC/auto_gen/euler_file_generator.f90 \
    -o $OBJ/euler_file_generator.o

$FCOMPILER -o $BIN/euler_file_generator $OBJ/*.o
rm -f $OBJ/euler_file_generator.o
cd $SRC/euler
./../../bin/euler_file_generator
cd $build_dir

echo "Compiling utility files ..."
$FCOMPILER $COMPILE_FLAG -J $MOD -c $SRC/utils/euler_utils.f90 \
    -o $OBJ/euler_utils.o
$FCOMPILER $COMPILE_FLAG -J $MOD -c $SRC/utils/euler_poker.f90 \
    -o $OBJ/euler_poker.o
$FCOMPILER $COMPILE_FLAG -J $MOD -c $SRC/utils/euler_primes.f90 \
    -o $OBJ/euler_primes.o
$FCOMPILER $COMPILE_FLAG -J $MOD -c $SRC/utils/euler_var_arr.f90 \
    -o $OBJ/euler_var_arr.o
$FCOMPILER $COMPILE_FLAG -J $MOD -c $SRC/utils/euler_lexical_sort.f90 \
    -o $OBJ/euler_lexical_sort.o
$FCOMPILER $COMPILE_FLAG -J $MOD -c $SRC/utils/euler_mi.f90 \
    -o $OBJ/euler_mi.o

echo "Testing euler_utils_m ..."
cd $build_dir
$FCOMPILER $COMPILE_FLAG -J $MOD -c \
    $SRC/tests/test_euler_utils.f90 \
    -o $OBJ/test_euler_utils.o
$FCOMPILER -o $BIN/euler_tests $OBJ/euler_utils.o $OBJ/test_euler_utils.o
./bin/euler_tests 
rm -f $OBJ/test_euler_utils.o

echo "Testing euler_mi_m ..."
$FCOMPILER $COMPILE_FLAG -J $MOD -c \
    $SRC/tests/test_euler_mi.f90 \
    -o $OBJ/test_euler_mi.o
$FCOMPILER -o $BIN/euler_mp_tests $OBJ/euler_mi.o $OBJ/test_euler_mi.o 
./bin/euler_mp_tests
rm -f $OBJ/test_euler_mi.o

echo "Compiling interface files ..."
$FCOMPILER $COMPILE_FLAG -J $MOD -c $SRC/euler/euler_interface.f90 \
    -o $OBJ/euler_interface.o

echo "Compiling problem files ..."
cd $SRC/probs/
EULER_PROBS=(*.f90)
for EULER_PROB in "${EULER_PROBS[@]}"
do
    echo "Compiling problem${EULER_PROB:11:4} ..."
    $FCOMPILER $COMPILE_FLAG -J $MOD -c $EULER_PROB \
        -o $OBJ/${EULER_PROB/".f90"/".o"}
done
cd ../../

echo "Compiling 'main.f90' ..."
$FCOMPILER $COMPILE_FLAG -J $MOD -c $SRC/euler/euler_prob_api.f90 \
    -o $OBJ/euler_prob_api.o
$FCOMPILER $COMPILE_FLAG -J $MOD -c $SRC/euler/euler_main.f90 \
    -o $OBJ/euler_main.o

echo "Generating binary file ..."
$FCOMPILER -o $BIN/euler_proj $OBJ/*.o

echo "Executing binary file ..."
mkdir ans && cd ./ans
./../bin/euler_proj

echo 
echo "Compiling and Executing is finished."
echo "The answers can be found in the folder 'ans'. Enjoy!"
echo "-- Captain Solo"