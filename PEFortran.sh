#! /usr/bin/env bash

# Number of problems
NPROB_MAX=60
VERSION="0.0.1"

# Directories
CUR=$(pwd)
SRC="${CUR}/src"
DAT="${CUR}/data"
PRB="${SRC}/probs"
UTL="${SRC}/utils"
BLD="${CUR}/build"

# Fortran compiler

for i in "$@"
do
case $i in
    -b=*|--build=*)
    BLD_OPT="${i#*=}"
    shift
    ;;
    -c=*|--compiler=*)
    FC="${i#*=}"
    shift
    ;;
    -n=*|--num_prob=*)
    NPROB="${i#*=}"
    shift
    ;;
    -d|--default)
    FC="gfortran"
    BLD_OPT="release"
    NPROB=${NPROB_MAX}
    shift
    ;;
    -h|--help)
    echo "Project Euler with Modern Fortran"
    echo "Version: ${VERSION}"
    echo "Flags possible:"
    echo "  -b=, --build=       Build options: release/debug"
    echo "  -c=, --compiler=    Compiler options: gfortran/ifort"
    echo "  -n=, --num_prob=    Number of problems: (max=${NPROB_MAX})"
    echo "  -d,  --default      This implies:"
    echo "                      --build=release --compiler=gfortran"
    echo "                      --num_prob=${NPROB_MAX}(currently solved)"
    echo "  -v,  --version      Check version."
    echo "  -r,  --remove       Remove build files and ANSWER.md."
    echo "  -h,  --help         To pop out this dialog."
    exit
    shift
    ;;
    -v|--version)
    echo ${VERSION}
    exit
    shift
    ;;
    -r|--remove)
    if [[ -d ${BLD} ]]; then
        rm -r ${BLD}
    else
        echo "Folder build doesn't exist."
    fi

    if [[ -f ANSWER.md ]]; then
        rm ANSWER.md
    else
        echo "File ANSWER.md doesn't exist."
    fi
    exit
esac
done

if [[ -z ${FC} ]]; then
    FC="gfortran"
fi

if [[ -z ${BLD_OPT} ]]; then
    BLD_OPT="release"
fi

if [[ -z "${NPROB}" ]]; then
    NPROB=${NPROB_MAX}
fi

echo "Project Euler with Modern Fortran"
echo "Version: " ${VERSION}
echo "Compiler used: ${FC}"
echo "Build option: ${BLD_OPT}"
echo "Number of problems tried: ${NPROB}"

if [[ ${FC:0:8} == "gfortran" ]] && [[ ${BLD_OPT} == "release" ]]; then
    FCFLAGS="-O3 -ffast-math -std=f2018"
elif [[ ${FC:0:8} == "gfortran" ]] && [[ ${BLD_OPT} == "debug" ]]; then
    FCFLAGS="-Wall -Wextra -fimplicit-none -fcheck=all -fbacktrace -std=f2018"
elif [[ ${FC:0:5} == "ifort" ]] && [[ ${BLD_OPT} == "release" ]]; then
    FCFLAGS="-O3 -xHost -ipo"
elif [[ ${FC:0:5} == "ifort" ]] && [[ ${BLD_OPT} == "debug" ]]; then
    FCFLAGS="-O0 -g -traceback"
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
TIME1=`date +%s`
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

echo "Compiling euler_main.f90..."
${COMPILE_F90} ${SRC}/euler_main.f90
echo "Creating executable..."
${FC} ${FCFLAGS} -o pe-fortran *.o
TIME2=`date +%s`
COMPILE_TIME=$((TIME2 - TIME1))
echo "Compile and precompile time: ${COMPILE_TIME} seconds"
echo "Executing Project Euler with Modern Fortran..."
./pe-fortran --compute-all

cd ${CUR}
cp ${BLD}/ANSWER.md .
echo "Copying ANSWER.md to project directory..."
echo "The project is successfully compiled/executed!"