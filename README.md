# Project Euler solutions written in Fortran

[Project Euler](https://projecteuler.net/about) is a math problem set. I occasionally solve PE problems for fun. Welcome!

## Getting Started

### Dependencies

| Dependencies          | Options               |
|:----------------------|:----------------------|
| Fortran 2008 compliant Compiler      | [gfortran](https://gcc.gnu.org/wiki/GFortran)/[ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html#gs.lki8b0) |
| Build Tool            | [Fortran Package Manager (FPM)](https://github.com/fortran-lang/fpm) |

### Run with FPM
For the basic usage, one should be able to run all the solved problems with a simple command
```bash
fpm run
```

## Euler toolkit

Please refer

## Contribution

If you would like to contribute:

1. add your file `problem_xxxx.f90` to `src/problems`, where `xxxx` is the problem number with leading zeros. The syntax should follow
```fortran
submodule(module_interface) submodule_eulerxxxx
implicit none
contains

module subroutine eulerxxxx(problem)
   !> Problem type
   type(problem_type), intent(inout) :: problem
   !> store your answer
   integer :: sln 
   !> Your solution here.
   !> ......
   !> write your answer to problem
   write (problem%answer, "(i20)") sln
end subroutine eulerxxxx
end submodule submodule_eulerxxxx
```
2. Update `*.inc` files (using bash)
```bash
./util/update_interface.sh
```
3. Use FPM to test
```
fpm test all
```

## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example Meson to wrap all the codes. 
- [x] Use a Fortran preprocessor to simplify my code.
- [ ] Write a documentation to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [x] Add version control.
- [x] Add a command line interface.
- [x] Add an installation script.
- [ ] Add GUI!
- [x] Use fpm to build, test, run and install the project.
- [x] Review exisiting code, rewrite/refactor code with:
   * Modern Fortran, 
   * Functional programming, and 
   * Array-oriented styles.
- [ ] ~~One of the three:~~
   * ~~Rewrite a simple yet robust big integer library.~~
   * ~~Use GMP for big integer.~~
   * ~~Use a good Fortran big integer library.~~
- [ ] Implement an associative array for Project Euler.