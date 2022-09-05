# Project Euler solutions written in Fortran

[Project Euler](https://projecteuler.net/about) is a math problem set. I occasionally solve PE problems for fun. Welcome!

## Getting Started

### Dependencies

| Dependencies          | Options               |
|:----------------------|:----------------------|
| Fortran Compiler      | [gfortran](https://gcc.gnu.org/wiki/GFortran)/[ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html#gs.lki8b0) |
| Build Tool            | [Meson](https://mesonbuild.com/)/[fpm](https://github.com/fortran-lang/fpm) |
| External Libraries    | [stdlib](https://github.com/fortran-lang/stdlib)    |
| Preprocessor          | [fypp](https://github.com/aradi/fypp)               |
| Formatter (optional)  | [fprettify](https://github.com/pseewald/fprettify)  |

All dependencies except stdlib are available on conda and can be installed with:

```
conda config --add channels conda-forge
conda create -n fpm fpm
conda activate fpm
conda install fypp fprettify # if build with fpm
# conda install fypp meson ninja # if build with meson
```

### Build with fpm

The fpm does not support fypp currently ([#78](https://github.com/fortran-lang/fpm/issues/78)), so in order to use fpm I wrote a simple installation script. If you have fpm, fypp and fprettify (optional) installed this would be the easist way to play with my project. For a quick start, navigate to the source directory and type:

```
./install.sh
```

Further information can be found through `./install.sh --help`.

### Build with meson

[Meson](https://mesonbuild.com/) is a fast and user friendly build tool. To build with Meson:

```
./install.sh --build-tool meson
```

### Tested compilers

| Compiler |
|:----|
| ifort (IFORT) 2021.5.0 20211109 |
| GNU Fortran (Ubuntu 11.2.0-7ubuntu2) 11.2.0 |

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
cloc --force-lang="Fortran 90",fypp .
```

To format all source files:
```
fprettify -i=2 -r src
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
- [ ] One of the three: 
   * Rewrite a simple yet robust big integer library.
   * Use GMP for big integer.
   * Use a good Fortran big integer library.
- [ ] Implement an associative array for Project Euler.