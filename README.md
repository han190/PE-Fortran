# Project Euler solutions written in Fortran

[Project Euler](https://projecteuler.net/about) is a problem set and I occasionally solve PE problems for fun. If you are looking for the same things, check it out!

## Getting Started

### Build with meson

* Dependencies: [Meson](https://mesonbuild.com/), [Ninja](https://ninja-build.org/), and a Fortran compiler ([gfortran](https://gcc.gnu.org/wiki/GFortran) and [ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html#gs.g8q0a5) are tested.)

```
meson build
meson test -C build
```

### Build with fpm

* Dependencies: [fpm](https://github.com/fortran-lang/fpm), [fypp](https://github.com/aradi/fypp), [fprettify](https://github.com/pseewald/fprettify) and a Fortran compiler ([gfortran](https://gcc.gnu.org/wiki/GFortran) and [ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html#gs.g8q0a5) are tested.)

The [fpm](https://github.com/fortran-lang/fpm) does not support fypp currently, so in order to use `fpm` I wrote a simple bash script to generate all fortran source files. Navigate to the source directory and type

```
./src-fpm.sh
```

and a folder named `src-fpm` will be generated. The generated source files will also be formatted by `fprettify`. Then one could simply use all the fpm commands,

```
fpm build
fpm test # if necessary
fpm run -- -f -a 60 -d $(realpath ./data)
```

* _For a minimum installation of the Intel Fortran compiler, take a look at [this discussion](https://fortran-lang.discourse.group/t/intel-releases-oneapi-toolkit-free-fortran-2018/471/35?u=han190)._


## Usage

```
$ ./PE-Fortran --help
PE Fortran Solution
Arguments:
   -v, --version          Print version.
   -h, --help             Pop up this message.
   -a N, --all N          Compute problem 1 through N.
   -p N, --problem N      Compute problem N.
   -f, --fancy            (optional) Use emojis to express
                          relative difficulties.
   -d, --data-directory   (optional) Directory of input data,
                          default is ".".
   -n, --number-of-trails (optional) Number of trails,
                          default is 1.
Example:
   (1) Compute problem 1 to 50, 10 trails per problem, with
       fancy style (emojis) and a specified data directory.
     $ PE-Fortran -f -a 50 -d $(realpath ./data) -n 10
```

## Misc

To count LOC:
```
cloc --force-lang="Fortran 90",fpp .
```

To format all source files:
```
fprettify -i=4 -r src
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