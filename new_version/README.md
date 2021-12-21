# Project Euler solutions written in Fortran

[Project Euler](https://projecteuler.net/about) is a problem set. I occasionally solve PE problems for fun. If you are looking for the same things, check it out!

## Getting Started

### Build with meson

Dependencies:
* [Meson](https://mesonbuild.com/)
* [Ninja](https://ninja-build.org/)
* Fortran compiler ([gfortran](https://gcc.gnu.org/wiki/GFortran) or [ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html#gs.g8q0a5))

```bash
meson build
meson test -C build
```

### Build with fpm

Dependencies:
* [fpm](https://github.com/fortran-lang/fpm)
* [fypp](https://github.com/aradi/fypp)
* [fprettify](https://github.com/pseewald/fprettify)
* Fortran compiler ([gfortran](https://gcc.gnu.org/wiki/GFortran) or [ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html#gs.g8q0a5))

The [fpm](https://github.com/fortran-lang/fpm) doesn't support fypp currently, I wrote a simple bash script to generate all fortran source files. Navigate to the source directory and type
```bash
./src-fpm.sh
```
and a folder named `src-fpm` will be generated, then we could use the `fpm` command
```bash
fpm build
fpm test # if necessary
fpm run -- -f -a 60 -d $(realpath ./data)
```
* _For a minimum installation of the Intel Fortran compiler, take a look at [this discussion](https://fortran-lang.discourse.group/t/intel-releases-oneapi-toolkit-free-fortran-2018/471/35?u=han190)._


## Usage

```bash
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

Usage:
   (1) Compute problem 1 through 50:
       PE-Fortran -a 50
   (2) Compute problem 1 through 50 with emoji output:
       PE-Fortran -f -a 50
   (3) Compute problem 50:
       PE-Fortran -p 50
   (4) Compute problem 1 to 50 with specified data path:
       PE-Fortran -f -a 50 -d $(realpath your/data/path)
```

## Misc

To count LOC:
```bash
cloc --force-lang="Fortran 90",fpp .
```

To format all source files:
```bash
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