#! /usr/bin/env bash

# Number of problems
# NPROB=58

# Directories
PWD=$(pwd)
SRC="${PWD}/src"
DAT="${PWD}/data"
PRB="${SRC}/probs"
UTL="${SRC}/utils"
BLD="${PWD}/build"

# Fortran compiler

for i in "$@"
do
case $i in
    -b=*|--build=*)
    BLD_OPT="${i#*=}"
    shift # past argument=value
    ;;
    -c=*|--compiler=*)
    FC="${i#*=}"
    shift # past argument=value
    ;;
    -n=*|--num_prob=*)
    NPROB="${i#*=}"
    shift # past argument=value
    ;;
    -d|--default)
    FC="gfortran"
    BLD_OPT="release"
    NPROB=58
    shift # past argument with no value
    ;;
    -h|--help)
    echo "Project Euler with Modern Fortran"
    echo 
    echo "Flags possible:"
    echo "  -b=, --build=       Build options: release/debug"
    echo "  -c=, --compiler=    Compiler options: gfortran/ifort"
    echo "  -n=, --num_prob=    Number of problems: 50(max=58)"
    echo "  -d , --default      This implies"
    echo "                      --build=release --compiler=gfortran"
    echo "                      --num_prob=58(currently solved)"
    echo "  -h , --help         To pop out this dialog."
    exit
    shift
esac
done

echo "Compiler used: ${FC}"
echo "Build option: ${BLD_OPT}"
echo "Number of problems tried: ${NPROB}"

if [[ ${FC} == "gfortran" ]] && [[ ${BLD_OPT} == "release" ]]; then
    FCFLAGS="-O3 -ffast-math -std=f2018"
elif [[ ${FC} == "gfortran" ]] && [[ ${BLD_OPT} == "debug" ]]; then
    FCFLAGS="-Wall -Wextra -fimplicit-none -fcheck=all -fbacktrace -std=f2018"
elif [[ ${FC} == "ifort" ]] && [[ ${BLD_OPT} == "release" ]]; then
    FCFLAGS="-O3 -xHost -ipo"
elif [[ ${FC} == "ifort" ]] && [[ ${BLD_OPT} == "debug" ]]; then
    FCFLAGS="-g -check all -fpe0 -warn -traceback -debug extended"
fi

COMPILE_F90="${FC} ${FCFLAGS} -c"
echo "Compiler flags used: FCFLAGS=${FCFLAGS}"

# Fortran preprocessor
FYPP="fypp"
FYPPFLAGS="-DNUM_PROB=${NPROB}"
COMPILE_FPP="${FYPP} ${FYPPFLAGS}"
# Test COMPILE_FPP
# echo ${COMPILE_FPP}
echo "fypp version: $(fypp --version)"

# Make directory build if it doesn't exist
# otherwise delete build and make a new one
if [[ -d "${BLD}" ]]; then
    rm -r build
fi
mkdir build

# File names
# Fypp files, utility files
FYPP_FILES=(euler_interface euler_prob_api)
UTIL_FILES=(euler_var_arr euler_utils euler_primes \
            euler_poker euler_mi euler_lexical_sort)

# Go to directory
cd ${BLD}

# Copy all the data files
cp ${DAT}/*.txt .

# Compile files in utils
echo "Compiling files in ./src/utils..."
for f in "${UTIL_FILES[@]}"; do
    ${COMPILE_F90} ${UTL}/${f}.f90
done

# Pre-process fypp files and generate mod and obj files
echo "Preprocessing and compiling files in ./src..."
for f in "${FYPP_FILES[@]}"; do
    ${COMPILE_FPP} ${SRC}/${f}.fypp ${f}.f90
    ${COMPILE_F90} ${f}.f90
done

# Genrate smod and obj files for all the problems
echo "Compiling files in ./src/probs..."
for f in "${PRB}/*"; do
    ${COMPILE_F90} ${f}
done

echo "Generating euler_main.f90..."
${COMPILE_FPP} ${SRC}/euler_main.fypp euler_main.f90
echo "Compiling euler_main.f90..."
${COMPILE_F90} euler_main.f90
echo "Creating executable..."
${FC} ${FCFLAGS} -o pe-fortran *.o
echo "Executing Project Euler with Modern Fortran..."
./pe-fortran --compute-all
echo "The project is successfully compiled/executed!"
