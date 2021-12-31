#!/usr/bin/env bash

srcdir=src
srcfpmdir=src-fpm
nproblem=$(ls ./$srcdir/euler/*.f90 | wc -l)
ntrails=1
fypp_flag=-DNUM_PROB=$nproblem

if ! command -v fypp &> /dev/null; then
    echo "Fypp not found, try conda install fypp."
    exit 0
fi

if ! command -v fpm &> /dev/null; then
    echo "Fpm not found, try conda install fpm."
    exit 0
fi

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

    for f in $srcdir/*.fpp; do
        filename=$(basename -- "$f")
        extension="${filename##*.}"
        filename="${filename%.*}"
        echo "Generating ${filename} through fypp..."
        fypp "${srcdir}/${filename}.fpp" "${srcfpmdir}/${filename}.f90" $fypp_flag
    done

    for d in $srcdir/*/; do
        foldername=$(basename -- "$d")
        echo "Copying folder ${foldername}..."
        cp -rf $srcdir/$foldername $srcfpmdir/$foldername
    done

    if command -v fprettify &> /dev/null; then
        echo "Found fprettify, formatting source codes..."
        fprettify -i=4 -r $srcfpmdir
    fi

    echo "Building PE-Fortran with '--profile debug'..."
    fpm build --profile debug
    echo "Testing modules with '--profile debug'..."
    fpm test --profile debug
    echo "Running PE-Fortran with '--profile debug'..."
    fpm run --profile debug -- -f -a $nproblem -n $ntrails -d $(realpath ./data)
    echo "Installing..."
    fpm install --flag -Ofast

fi

if [ $? -eq 0 ]; then
    echo "Project built successfully."
fi
