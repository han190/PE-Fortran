# Project Euler with Modern Fortran

[Project Euler](https://projecteuler.net/about) is a weekly updated math problem set. I created this project mainly because I want to practice my Fortran programming skills, so although not necessary, everything is written by modern Fortran. If you are learning Fortran, this project might be helpful.

## Dependencies

You will need a Fortran compiler to run this project. The [GNU Fortran compiler](https://gcc.gnu.org/fortran/) is recommended. The code is tested with gfortran (version >= 9.0, since the Fortran 2008 feature [findloc](https://gcc.gnu.org/onlinedocs/gfortran/FINDLOC.html) is used in problem 54) on Mac, Linux, and the WSL system. To install the GNU fortran compiler, 

on **Mac OS** simply use [brew](https://brew.sh/),

```shell
brew install gfortran
```

For **Fedora** users,

```shell
sudo dnf install gfortran
```

For **Ubuntu** users,

```shell
sudo apt install gfortran
```

On **Windows** it is a bit complicated. First you will need to enable the WSL system, and then build GNU fortran (version >= 9.0) from scratch because the default gfortran version on WSL is 7.4.0. [This article](https://solarianprogrammer.com/2017/05/04/building-gcc-wsl-windows-subsystem-linux/) might be helpful.

## Compiling and executing the program

 To compile and execute the code, simply run

```shell
./build.sh
```

 You can also enable the `debug` option

 ```shell
 ./build.sh --debug
 ```

 or the `optimize` option, which compiles everything with `-O3`

 ```shell 
 ./build.sh --optimize
 ```

On the WSL system, 

```shell
./build.sh --WSL /path/to/gfortran9/bin
```

you can also add debug or optimization flag with it

```shell
./build.sh --optimize --WSL /path/to/gfortran9/bin
```

To clean all generated files,

```shell
./clean_build.sh
```

(The shell script is written for the GNU Fortran compiler, if you want to use the [Intel Fortran compiler](https://software.intel.com/en-us/fortran-compilers) you might have to write a build file yourself.)

## A very short introduction

The structure of the program is presented here:

```bash
src
├── euler
│   └── euler_main.f90
├── interface_auto_gen
│   ├── euler_file_generator.f90
│   └── euler_file_generator_m.f90
├── probs
│   ├── euler_prob_0001.f90
│   ├── euler_prob_0002.f90
│   ├── euler_prob_0003.f90
│   └── ... ...
├── tests
│   ├── test_euler_utils.f90
│   ├── test_euler_mi.f90
└── utils
    ├── euler_lexical_sort.f90
    ├── euler_mpf.f90
    ├── euler_primes.f90
    ├── euler_utils.f90
    └── euler_var_arr.f90
```

The folder `utils` provides all the utilities required for this project and they are tested by the code `test_euler_utils.f90` in the folder `tests`. Each problem in the folder `probs` is a submodule of the module `euler_interface_m` that will be automatically generated and put into the folder `src/euler`.

## A summary of results

Since it is aimed as a Fortran practice project, the performance of the code is NOT a priority.The `Tspan` for each problem in the output file `ans/README.md` is the time it takes to call the corresponding problem function:

```fortran
do i = 1, nop
    call cpu_time(t_i)
    ans(i) = probs(i)%euler_prob_p()
    call cpu_time(t_f)
    tspan(i) = t_f - t_i
end do
```

However, some of the utilities required has been compiled before all the submodules(problems) are compiled and the time spent on that is not counted, so the total time spent by running `time ./build.sh` is also presented in the little table below.

|Specs of my computer                                           |
|:--------------------------------------------------------------|
|Compiler: GNU Fortran (GCC) 9.2.1 20190827 (Red Hat 9.2.1-1)   |
|CPU: Intel Core i5-8250U @ 8x 3.4GHz                           |
|RAM: 2350MiB / 7718MiB                                         |

|Results                            |                   |
|:----------------------------------|:------------------|
| Problems solved                   |   56              |    
| Total time spent                  |   0.876871(s)     |
| Average time spent per problem    |   0.015658(s)     |
| `time ./build.sh`                 |   5.198(s) total  |

## A todo list

Here is what I plan to do in the future.

- [x] Organize folders, use Shell scripts to compile codes.
- [ ] Use a build tool, for example [Meson](https://mesonbuild.com/) to wrap
    all the codes.
- [ ] Write a wiki page to clearly explain the algorithms, Fortran features, or
    anything that is interesting for each question.
