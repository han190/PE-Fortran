# Project Euler solutions written in Fortran

[Project Euler](https://projecteuler.net/about) is a math problem set. I occasionally solve PE problems for fun. Welcome!

## Getting Started

### Dependencies

| Dependencies          | Options               |
|:----------------------|:----------------------|
| Fortran 2008 compliant Compiler      | [gfortran](https://gcc.gnu.org/wiki/GFortran)/[ifx](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html#gs.lki8b0) |
| Build Tool            | [Fortran Package Manager (FPM)](https://github.com/fortran-lang/fpm) |

### Run with FPM
#### Solve a single problem
```bash
fpm run [-- p<problem number> [--trail <number of trails/problem>]]
# Example1, solve all problems: fpm run
# Example2, solve all problems 10 times: fpm run -- --trail 10
# Example3, solve problem 10: fpm run -- p10
# Example4, solve problem 10 for 100 times: fpm run -- p10 --trail 10
```
#### To avoid Memory Sanitizer from the Intel Fortran compiler(ifx (IFX) 2023.2.0)
```bash
fpm run --compiler ifx --flag "-check all,nouninit"
```
* `fpm run -- --help` for more information.

## Contribution
If you would like to contribute:
#### Step 1
Add your file `problem_xxxx.f90` to `src/problems`, where `xxxx` is the problem number with leading zeros. If a data file is required, add your data file `data_xxxx.txt` to `data/`. The syntax of a solution submodule should follow
```fortran
submodule(module_problem) submodule_eulerxxxx
implicit none
contains

module subroutine eulerxxxx(problem)
   class(problem_type), intent(inout) :: problem !> Problem type
   integer :: sln !> store your answer
   !> If data file is required:
   !> open (newunit=unit, file=problem%file)
   write (problem%answer, "(i20)") sln
end subroutine eulerxxxx
end submodule submodule_eulerxxxx
```

#### Step 2
Update `*.inc` files by using 
```bash
fpm run preprocess
```