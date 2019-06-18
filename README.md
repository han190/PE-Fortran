# Project Euler with modern Fortran

[Project Euler](https://projecteuler.net/about) is a weekly updated math problem set. I created this project mainly because I wanted to practice my Fortran programming skills, so although not necessary, everything is written by Modern Fortran. If you are learning Fortran, this project might be helpful.

Since it is aimed as a Fortran practice project, performance of the code is NOT a priority. The so called "benchmark" given in the folder _answer_ is barely a demonstration of time span. So do not expect too much even though it is written in Fortran.

# Compile and run 

The code is tested with [Intel Fortran compiler](https://software.intel.com/en-us/fortran-compilers) and [GNU Fortran compiler](https://gcc.gnu.org/fortran/) on Mac and Linux. To compile and run the code, simply go to the folder _answer_ and 
```
./euler_gfortran.sh
```
or
```
./euler_ifort.sh
```

# Todo list
Here is what I plan to do in the future. 
- [] Use a build tool, for example [Meson](https://mesonbuild.com/), to wrap all the codes.
- [] Write a wiki page to clearly explain the algorithms, Fortran features, or anything that is interesting for each question. 

# Results and benchmarks

The results are [here](https://gitlab.com/CaptainSolo/project-euler/tree/master/answer). 

Have fun!