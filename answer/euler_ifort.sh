#!/bin/bash
cp ../data/*.txt .
ifort -O3 -c ../utilities/euler_utilities.f90
ifort -O3 -c ../utilities/euler_primes.f90
ifort -O3 -c ../utilities/euler_lexical_sort.f90 
ifort -O3 -c ../problems/euler_problems.f90
ifort -O3 -c ../problems/euler_prob_****.f90
ifort -O3 -c euler_main.f90
ifort -O3 -o euler *.o
./euler
rm *.o
rm *.mod
rm *.smod
rm euler
rm *.txt
