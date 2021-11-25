# Project Euler solutions written in Fortran

[Project Euler](https://projecteuler.net/about) is a problem set. I solve PE problems to polish my Fortran skills, expand my math knowledge base and most importantly FOR FUN. If you are looking for the same things, check it out!

## Getting Started

### Prerequistes

You will need a Fortran compiler (The tested ones are [gfortran](https://gcc.gnu.org/wiki/GFortran) and [ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html#gs.g8q0a5).) and the [Fortran Package Manager (fpm)](https://github.com/fortran-lang/fpm) to compile/run/test/install this project. For a minimum installation of the Intel Fortran compiler, take a look at [this discussion](https://fortran-lang.discourse.group/t/intel-releases-oneapi-toolkit-free-fortran-2018/471/35?u=han190).

### Build with fpm

To build this project with fpm, navigate to the root directory of this repo and

```bash
fpm build
```

and run the project with

```bash
fpm run -- --fancy --all <number of problems>
```

You could also install it by (the default `PREFIX` is `$HOME/.local/bin`)

```bash
fpm install
```

## Usage

```
$ PE-Fortran --help
PE Fortran Solution
Arguments:
   -v, --version          Print version.
   -h, --help             Pop up this message.
   -f, --fancy            (optional) Use emojis to express
                          relative difficulties.
   -a N, --all N          Compute problem 1 through N.
   -p N, --problem N      Compute problem N.

Usage:
   (1) Compute problem 1 through 50:
       PE-Fortran --all 50
   (2) Compute problem 1 through 50 with emoji output:
       PE-Fortran --fancy --all 50
   (3) Compute problem 50:
       PE-Fortran --problem 50
```

## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example Meson to wrap all the codes. 
- [x] Use a Fortran preprocessor to simplify my code.
- [ ] Write a documentation to clearly explain the algorithms, Fortran features, or anything that is interesting for each question. (A temporary one is available here [https://han190.github.io/PE-Fortran/](https://han190.github.io/PE-Fortran/).)
- [x] Add version control.
- [ ] When I finish 75 problems, write a command line interface. When I finish 100 problems (if that is possible), write a GUI.
- [x] Use fpm to build, test, run and install the project.
- [ ] Review exisiting code, rewrite/refactor code with (1) modern Fortran, (2) functional programming and (3) array-oriented styles.