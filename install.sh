#!/usr/bin/env bash

srcdir=src
srcfpmdir=src-fpm
nproblem=$(ls ./$srcdir/euler/*.f90 | wc -l)
ntrails=1
fypp_flag=-DNUM_PROB=$nproblem
profile=release
FC=gfortran
GSO=false

help_message() {
    echo "PE-Fortran Installation Script"
    echo "(1) Quick Start:"
    echo "    $ ./install.sh"
    echo
    echo "(2) Available Arguments:"
    echo "    -h,    --help                  Pop up help message."
    echo "    -c,    --compiler              Fortran compiler, default is gfortran."
    echo "    -g,    --generate-source-only  Generate source code using fypp, if the"
    echo "                                   flag is applied, -c, -p, are ignored."
    echo "    -sd,   --src-dir               Source directory, default is 'src'."
    echo "    -sfd,  --src-fpm-dir           FPM directory, default is 'src-fpm'."
    echo "    -np,   --number-of-problems    Number of problems, default is the"
    echo "                                   number of files in src/euler/."
    echo "    -nt,   --number-of-trails      Number of trails, default is 1."
    echo "    -p, --profile                  Fypp flags, default is"
    echo "                                   '-DNUM_PROB=\$-np'"
    echo "    -fpm,  --fpm-flag              FPM flags, default is "
    echo "                                   '--profile release'."
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
    -c | --compiler)
        FC=$2
        shift 2
        ;;
    -g | --generate-source-only)
        GSO=true
        shift 1
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
    -p | --profile)
        fypp_flag="-DNUM_PROB=$nproblem"
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

    if [ $GSO == true ]; then
        exit 0
    fi

    echo "Building PE-Fortran with '--profile $profile'..."
    fpm build --compiler $FC --profile $profile
    echo "Testing modules with '--profile $profile'..."
    fpm test --compiler $FC --profile $profile
    echo "Testing PE-Fortran with '--profile $profile'..."
    echo
    fpm run --compiler $FC --profile $profile -- --version
    fpm run --compiler $FC --profile $profile -- \
        --fancy --all $nproblem \
        --number-of-trails $ntrails \
        --data-directory $(realpath ./data)
    echo "Installing..."
    fpm install --compiler $FC --profile $profile

fi

if [ $? -eq 0 ]; then
    echo "Project built successfully."
fi
