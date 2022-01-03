#!/usr/bin/env bash

srcdir=src
srcfpmdir=src-fpm
nproblem=$(ls ./$srcdir/euler/*.f90 | wc -l)
ntrails=1
fypp_flag=-DNUM_PROB=$nproblem
fpm_flag="--profile release"

help_message() {
    echo "PE-Fortran Installation Script"
    echo "(1) Quick Start:"
    echo "    $ ./install.sh"
    echo
    echo "(2) Available Arguments:"
    echo "    -h,    --help                  Pop up help message."
    echo "    -sd,   --src-dir               Source directory, default is 'src'."
    echo "    -sfd,  --src-fpm-dir           FPM directory, default is 'src-fpm'."
    echo "    -np,   --number-of-problems    Number of problems, default is the"
    echo "                                   number of files in src/euler/."
    echo "    -nt,   --number-of-trails      Number of trails, default is 1."
    echo "    -fypp, --fypp-flag             Fypp flags, default is"
    echo "                                  '-DNUM_PROB=\$-np'"
    echo "    -fpm,  --fpm-flag              FPM flags, default is "
    echo "                                  '--profile release'."
}

check_dependency() {
    executable=$1
    if ! command -v $executable &>/dev/null; then
        msg = "[ERROR] $executable not found, \
            try: conda install $executable."
        echo $msg 
        exit 0
    fi
}

while true; do
    case "$1" in
    -h | --help)
        help_message
        exit 1
        ;;
    -sd | --src-dir)
        srcdir=$2
        shift 2
        ;;
    -sfd | --src-fpm-dir)
        srcfpmdir=$2
        shift 2
        ;;
    -np | --number-of-problems)
        nproblem=$2
        shift 2
        ;;
    -nt | --number-of-trails)
        ntrails=$2
        shift 2
        ;;
    -fypp | --fypp-flag)
        fypp_flag="-DNUM_PROB=$nproblem"
        shift 2
        ;;
    -fpm | --fpm-flag)
        fpm_flag="--profile release"
        shift 2
        ;;
    *)
        break
        ;;
    esac
done

check_dependency fypp
check_dependency fpm

if [ -d "src" ]; then

    if [ -d "src-fpm" ]; then
        echo "'src-fpm' already exists, deleting it..."
        rm -rf src-fpm
    fi
    echo "Creating folder 'src-fpm'..."
    mkdir -p src-fpm

    if [ -d "build" ]; then
        echo "'build' already exists, deleting it..."
        rm -rf build
    fi
    echo "Creating folder 'build'..."
    mkdir -p build

    for f in $srcdir/*.f90; do
        filename=$(basename -- "$f")
        echo "Copying ${filename}..."
        cp $srcdir/$filename $srcfpmdir/$filename
    done

    for f in $srcdir/*.fypp; do
        filename=$(basename -- "$f")
        extension="${filename##*.}"
        filename="${filename%.*}"
        echo "Generating ${filename} through fypp..."
        fypp "${srcdir}/${filename}.fypp" \
            "${srcfpmdir}/${filename}.f90" $fypp_flag
    done

    for d in $srcdir/*/; do
        foldername=$(basename -- "$d")
        echo "Copying folder ${foldername}..."
        cp -rf $srcdir/$foldername $srcfpmdir/$foldername
    done

    if command -v fprettify &>/dev/null; then
        echo "Found fprettify, formatting source codes..."
        fprettify -i=4 -r $srcfpmdir
    else
        echo "fprettify not found."
    fi

    echo "Building PE-Fortran with '$fpm_flag'..."
    fpm build $fpm_flag
    echo "Testing modules with '$fpm_flag'..."
    fpm test $fpm_flag
    echo "Running PE-Fortran with '$fpm_flag'..."
    echo
    fpm run $fpm_flag -- --version
    fpm run $fpm_flag -- \
        --fancy --all $nproblem \
        --number-of-trails $ntrails \
        --data-directory $(realpath ./data)
    echo "Installing..."
    fpm install $fpm_flag

fi

if [ $? -eq 0 ]; then
    echo "Project built successfully."
fi
