#! /usr/bin/env bash

# Number of problems
NPROB=58

# Directories
PWD=$(pwd)
SRC="${PWD}/src"
DAT="${PWD}/data"
PRB="${SRC}/probs"
UTL="${SRC}/utils"
BLD="${PWD}/build"

# Fortran compiler
FC="gfortran"
FCFLAGS="-O3 -ffast-math"
# FCFLAGS="-Wall -Wextra -fimplicit-none -fcheck=all -fbacktrace"
COMPILE_F90="${FC} ${FCFLAGS} -c"
# Test COMPILE_F90
# echo ${COMPILE_F90}

# Fortran preprocessor
FYPP="fypp"
FYPPFLAGS="-DNUM_PROB=${NPROB}"
COMPILE_FPP="${FYPP} ${FYPPFLAGS}"
# Test COMPILE_FPP
# echo ${COMPILE_FPP}

# Make directory build if it doesn't exist
# otherwise delete build and make a new one
if [ -d "${BLD}" ]; then
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
for f in "${UTIL_FILES[@]}"; do
    ${COMPILE_F90} ${UTL}/${f}.f90
done

# Pre-process fypp files and generate mod and obj files
for f in "${FYPP_FILES[@]}"; do
    ${COMPILE_FPP} ${SRC}/${f}.fypp ${f}.f90
    ${COMPILE_F90} ${f}.f90
done

# Genrate smod and obj files for all the problems
for f in "${PRB}/*"; do
    ${COMPILE_F90} ${f}
done

${COMPILE_F90} ${SRC}/euler_main.f90
${FC} ${FCFLAGS} -o pefortran *.o
