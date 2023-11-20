#!/usr/bin/env bash
compiler=gfortran
builddir=./build/
utildir=./util/

rm -rf $builddir
mkdir -p $builddir
ls ./src/problems/problem_????.f90 > $builddir/solved_problems.txt
ls ./data/data_????.txt > $builddir/required_datasets.txt
$compiler $utildir/preprocess.f90 -o $builddir/update
$builddir/update