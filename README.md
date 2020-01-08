# Project Euler with Modern Fortran

[Project Euler](https://projecteuler.net/about) is a weekly updated math problem set. I created this project mainly because I wanted to practice my Fortran programming skills, so although not necessary, everything is written by modern Fortran. If you are learning Fortran, this project might be helpful.

## Dependencies

You will need a Fortran compiler to run this project. The [GNU Fortran compiler](https://gcc.gnu.org/fortran/) is recommended.

## Compiling and executing the program

The code is tested with gfortran (version >= 6.0) on Mac and Linux. To compile and execute the code, simply run

```shell
./build.sh
```

To clean it,

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
│   └── test_euler_utils.f90
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
|CPU: Intel Core i7-8700K @ 12x 4.7GHz [27.8°C]                 |
|RAM: 2355MiB / 31968MiB                                        |

|Results                            |                   |
|:----------------------------------|:------------------|
| Problems solved                   |   56              |    
| Total time spent                  |   0.576915(s)     |
| Average time spent per problem    |   0.010302(s)     |
| `time ./build.sh`                 |   3.275(s) total  |

## A todo list

Here is what I plan to do in the future.

- [x] Organize folders, use Shell scripts to compile codes.
- [ ] Test code with Intel Fortran compiler. Write a wrapper for it.
- [ ] Use a build tool, for example [Meson](https://mesonbuild.com/) to wrap
    all the codes.
- [ ] Write a wiki page to clearly explain the algorithms, Fortran features, or
    anything that is interesting for each question.
