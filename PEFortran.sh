#! /usr/bin/env bash

# a function that checks if a command exit
checkIfCommandExists () {
    echo "Looking for $1..."
    if command -v $1 >/dev/null 2>&1 ; then
        echo "$1 found."
    else 
        echo "[STOPPED] $1 not found. Please install $1 first."
        exit
    fi
}

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

# Flags available
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
            BLD_OPT="optimize"
            NPROB=${NPROB_MAX}
            shift
        ;;
        -h|--help)
            echo "Project Euler with Modern Fortran"
            echo "Version: ${VERSION}"
            echo "Flags possible:"
            echo "  -b=, --build=       Build options: optimize/debug"
            echo "  -c=, --compiler=    Compiler options: gfortran/ifort/flang"
            echo "  -n=, --num_prob=    Number of problems: (max=${NPROB_MAX})"
            echo "  -d,  --default      This implies:"
            echo "                      --build=optimize --compiler=gfortran"
            echo "                      --num_prob=${NPROB_MAX}"
            echo "  -v,  --version      Check version."
            echo "  -r,  --remove       Remove build files and ANSWER.md."
            echo "  -h,  --help         Pop out this message."
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
    BLD_OPT="optimize"
fi

if [[ -z "${NPROB}" ]]; then
    NPROB=${NPROB_MAX}
fi

echo "Project Euler with Modern Fortran"
echo "Version: " ${VERSION}
checkIfCommandExists ${FC}
echo "Build option: ${BLD_OPT}"
echo "Number of problems tried: ${NPROB}"

if [[ ${FC:0:8} == "gfortran" ]] && [[ ${BLD_OPT} == "optimize" ]]; then
    FCFLAGS="-O3 -ffast-math -std=f2018"
    elif [[ ${FC:0:8} == "gfortran" ]] && [[ ${BLD_OPT} == "debug" ]]; then
    FCFLAGS="-Wall -Wextra -fimplicit-none -fcheck=all -fbacktrace -std=f2018"
    elif [[ ${FC:0:5} == "ifort" ]] && [[ ${BLD_OPT} == "optimize" ]]; then
    FCFLAGS="-O3 -xHost -ipo"
    elif [[ ${FC:0:5} == "ifort" ]] && [[ ${BLD_OPT} == "debug" ]]; then
    FCFLAGS="-O0 -g -traceback -debug all -check all"
    elif [[ ${FC:0:5} == "flang" ]] && [[ ${BLD_OPT} == "optimize" ]]; then
    FCFLAGS="-O3 -ffast-math -std=f2018"
    elif [[ ${FC:0:5} == "flang" ]] && [[ ${BLD_OPT} == "debug" ]]; then
    FCFLAGS="-O0 -Wall -Wextra -fdebug-measure-parse-tree"
fi

COMPILE_F90="${FC} ${FCFLAGS} -c"
echo "Compiler flags used: FCFLAGS=${FCFLAGS}"

# Fortran preprocessor
FYPP="fypp"
checkIfCommandExists ${FYPP}
FYPPFLAGS="-DNUM_PROB=${NPROB}"
COMPILE_FPP="${FYPP} ${FYPPFLAGS}"

# Make directory build if it doesn't exist
# otherwise delete build and make a new one
if [[ -d "${BLD}" ]]; then
    rm -r build
fi
mkdir build

# File names
# Fypp files, utility files
FYPP_FILES=(euler_interface euler_prob_api)
UTIL_FILES=(                    \
    euler_var_arr euler_utils   \
    euler_primes euler_mi       \
)

# Go to directory and copy all the data files
cd ${BLD}
cp ${DAT}/*.txt .

# Compile files in utils
TIME_START=`date +%s`
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
for i in $(seq -f "%04g" $NPROB); do
    echo "Compiling problem ${i}..."
    ${COMPILE_F90} ${PRB}/euler_prob_${i}.f90
done

echo "Compiling euler_main.f90..."
${COMPILE_F90} ${SRC}/euler_main.f90
echo "Creating executable..."
${FC} ${FCFLAGS} -o pe-fortran *.o
TIME_END=`date +%s`
COMPILE_TIME=$((TIME_END - TIME_START))
echo "Compile and precompile time: ${COMPILE_TIME} seconds"
echo "Executing Project Euler with Modern Fortran..."
./pe-fortran --compute-all

cd ${CUR}
cp ${BLD}/ANSWER.md .
echo "Copying ANSWER.md to project directory..."
echo "The compilation and execution is completed!"