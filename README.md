# Project Euler with Modern Fortran

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

- [x] Organize folders, use Shell scripts to compile codes.  
- [ ] Use a build tool, for example [Meson](https://mesonbuild.com/), to wrap all the codes.
- [ ] Write a wiki page to clearly explain the algorithms, Fortran features, or anything that is interesting for each question. 

# Results and benchmarks
In case you are interested in comparing performances.
* OS: Fedora 30 Thirty
* Kernel: x86_64 Linux 5.1.11-300.fc30.x86_64
* CPU: Intel Core i7-8700K @ 12x 4.7GHz [27.8°C]
* RAM: 2157MiB / 15889MiB
* Compiler: GNU Fortran (GCC) 9.1.1 20190503 (Red Hat 9.1.1-1)

The results are [here](https://github.com/han190/Project-Euler-with-Modern-Fortran/blob/master/answer/README.md). Have fun!
