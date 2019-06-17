# Project Euler with Modern Fortran

[Project Euler](https://projecteuler.net/about) is a weekly updated math problem set. I created this project mainly because I wanted to practice my Fortran programming skills, so although not necessary, everything is written by modern Fortran. Some new features in modern Fortran. For example, the structure [submodule](http://fortranwiki.org/fortran/show/Submodules) is used to reduce the source code size of the module proper and avoid the so called compilation cascades. If you are learning Fortran, this might give you an idea of how the new features are implemented. 

Since it is aimed as a Fortran practice project, performance of the code is NOT a priority. The so called "benchmark" given below is barely a demonstration of time span. So do not expect too much even though it is written in Fortran.

# Compile and run 

The code is tested with [Intel Fortran compiler](https://software.intel.com/en-us/fortran-compilers) and [GNU Fortran compiler](https://gcc.gnu.org/fortran/). To compile and run the code, simply go to the folder Answer and 
```
./euler_gfortran.sh
```
or
```
./euler_ifort.sh
```
* I don't know too much about build tools. Currently all the files are wrapped and built by my poorly written Shell scripts. I am planning to learn and use [Meson](https://mesonbuild.com/) in the future.

# Results and benchmarks

The results are [here](https://gitlab.com/CaptainSolo/project-euler/tree/master/answer). 

Have fun!