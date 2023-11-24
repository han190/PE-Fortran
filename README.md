# Project Euler solutions written in Fortran

[Project Euler](https://projecteuler.net/about) is a math problem set. I occasionally solve PE problems for fun. Welcome!

## Getting Started

### Dependencies

| Dependencies          | Options               |
|:----------------------|:----------------------|
| Fortran 2008 compliant Compiler      | [gfortran](https://gcc.gnu.org/wiki/GFortran)/[ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html#gs.lki8b0) |
| Build Tool            | [Fortran Package Manager (FPM)](https://github.com/fortran-lang/fpm) |

### Run with FPM
To run a single problem:
```bash
fpm run -- P<Problem number> [T<Number of trails>]
```
To run all solved problems:
```bash
fpm run [-- T<Number of trails>]
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
Update `*.inc` files by using `bash ./util/update.sh` or `.\util\update.ps1` for Unix-like/Windows system correspondingly. The Fortran script `util/preprocess.f90` will scan `./data/` and `./src/problems` to generate an array of solved problems, and based on that it generates `*.inc` files required by `module_problem`.

## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [ ] Write a documentation to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [ ] Implement an associative array for Project Euler.