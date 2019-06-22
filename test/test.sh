#!/bin/bash 
gfortran -O3 -c ../utilities/euler_utilities.f90
gfortran -O3 -c ../utilities/euler_primes.f90
gfortran -O3 -c ../utilities/euler_lexical_sort.f90 
gfortran -c test.f90 
gfortran -o play *.o 
./play 
rm *.o 
rm *.mod 
rm *.smod 
rm play 