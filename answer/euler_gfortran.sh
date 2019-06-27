#!/bin/bash
cp ../data/*.txt . # copy all the data here
gfortran -O3 -c ../utilities/euler_utilities.f90 # compile utilities.f90
gfortran -O3 -c ../utilities/euler_primes.f90 # compile submodules
gfortran -O3 -c ../utilities/euler_lexical_sort.f90 # compile submodules
gfortran -O3 -c ../problems/euler_problems.f90 # compile problems.f90
gfortran -O3 -c ../problems/euler_prob_****.f90 # compile submodules
gfortran -O3 -c euler_main.f90 # compile main.f90
gfortran -O3 -o euler *.o # compile all 
./euler # run 
rm *.o # remove unecessary files
rm *.mod # remove unecessary files
rm *.smod # remove unecessary files
rm euler # remove unecessary files
rm *.txt # remove unecessary files
