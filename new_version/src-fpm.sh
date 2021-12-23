#!/usr/bin/env bash

srcdir=src
srcfpmdir=src-fpm
nproblem=60
fypp_flag=-DNUM_PROB=$nproblem

if [ -d "src" ]; then

    if [ -d "src-fpm" ]; then
        echo "'src-fpm' already exists, deleting it..."
        rm -rf src-fpm
    fi
    echo "Creating folder 'src-fpm'..."
    mkdir -p src-fpm

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

    echo "Formatting source codes..."
    fprettify -i=4 -r $srcfpmdir

fi