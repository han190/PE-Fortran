# Project Euler with Modern Fortran

[Project Euler](https://projecteuler.net/about) is a weekly updated math problem set. I created this project mainly because I want to practice my Fortran programming skills, so although not necessary, everything is written by modern Fortran. If you are learning Fortran, this project might be helpful.

You will need a Fortran compiler to run this project. The [GNU Fortran compiler](https://gcc.gnu.org/fortran/) is recommended. The code is tested with gfortran (version >= 9.0, since the Fortran 2008 feature [findloc](https://gcc.gnu.org/onlinedocs/gfortran/FINDLOC.html) is used in problem 54) on Mac and Linux. Also, this project uses [fypp](https://github.com/aradi/fypp) as the preprocessor. To install fypp the easist way is `pip install fypp`.

## Compiling and executing the program

To build the project,

```shell
./install.sh && cd build && ./pefortran --compute-all
```

the answers can be found in `build/ANSWER.md`.

## A summary of results

Since it is aimed as a language learning project, the performance of the code is NOT a priority. The `Tspan` for each problem in the output file `ANSWER.md` indicate the time it takes to call the corresponding problem function and `probs` is a procedure pointer array.

```fortran
do i = 1, nop
    call cpu_time(t_i)
    ans(i) = probs(i)%euler_prob_p()
    call cpu_time(t_f)
    tspan(i) = t_f - t_i
end do
```


## Benchmarks

| Summary                        |               |
|:-------------------------------|:--------------|
| Problems solved                |   56          |
| Total time spent               |   0.541019(s) |
| Average time spent per problem |   0.009661(s) |

with my computer 

|Specs of my computer                                           |
|:--------------------------------------------------------------|
|Compiler: GNU Fortran (Ubuntu 10.2.0-13ubuntu1) 10.2.0         |
|CPU: Intel Core i7-8700K @ 12x 4.7GHz                          |
|RAM: 31965MiB                                                  |


## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example [Meson](https://mesonbuild.com/) to wrap all the codes. (PS1: Crappy but I did it...) (PS2: I deleted it since it is not necessary to use a build tool with such a small project.)
- [x] Use some Fortran preprocessor to simplify my code.
- [ ] Write a wiki page to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [ ] When I finish 75 problems, write a command line interface. When I finish 100 problems, write a GUI.
