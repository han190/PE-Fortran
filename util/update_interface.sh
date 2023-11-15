#!/usr/bin/env bash
compiler=gfortran
builddir=./build/
utildir=./util/

rm -rf $builddir
mkdir $builddir
$compiler -cpp $utildir/interface.f90 -o $builddir/interface
$builddir/interface