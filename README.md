# Project Euler with Modern Fortran

[Project Euler](https://projecteuler.net/about) is a weekly updated math problem set. I created this project mainly because I want to practice my Fortran programming skills, so although not necessary, everything is written by modern Fortran. If you are learning Fortran, this project might be helpful.

## Requirements

You will need a Fortran compiler to run this project. The [GNU Fortran compiler](https://gcc.gnu.org/fortran/) is recommended. The code is tested with gfortran (version >= 9.0, since the Fortran 2008 feature [findloc](https://gcc.gnu.org/onlinedocs/gfortran/FINDLOC.html) is used in problem 54) on Mac and Linux (Fedora). Also, the [Meson](https://mesonbuild.com/index.html) Build system (>= 0.53) is used to compile this project.

## Compiling and executing the program

To build the project,

```shell
meson build
cd build
ninja
```

and then run the binary file

```shell
./euler
```

## A summary of results

Since it is aimed as a Fortran practice project, the performance of the code is NOT a priority.The `Tspan` for each problem in the output file `README.md` is the time it takes to call the corresponding problem function:

```fortran
do i = 1, nop
    call cpu_time(t_i)
    ans(i) = probs(i)%euler_prob_p()
    call cpu_time(t_f)
    tspan(i) = t_f - t_i
end do
```

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

## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example [Meson](https://mesonbuild.com/) to wrap
    all the codes. (Crappy but I did it...)
- [ ] Write a wiki page to clearly explain the algorithms, Fortran features, or
    anything that is interesting for each question.
