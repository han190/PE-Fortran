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

## Contribution
If you would like to contribute:
#### Step 1
Add your file `problem_xxxx.f90` to `src/problems`, where `xxxx` is the problem number with leading zeros. The syntax of a solution submodule should follow
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

#### Step 2
Update `*.inc` files (using bash). The Fortran script `util/interface_generator.f90` scans `./data/` and `./src/problems` to generate an array of solved problems, and based on that it generates `*.inc` files required by `module_problem`.
```bash
./util/update_interface.sh
```

#### Step 3
Test 
```
fpm test all
```

## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [ ] Write a documentation to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [ ] Implement an associative array for Project Euler.