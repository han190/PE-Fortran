# Project Euler solutions written in Fortran

[Project Euler](https://projecteuler.net/about) is a math problem set. I occasionally solve PE problems for fun. Welcome!

## Getting Started

### Dependencies

All dependencies are available on conda and can be installed with:

```
conda config --add channels conda-forge
conda create -n fpm fpm
conda activate fpm
conda install fypp fprettify # if build with fpm
# conda install fypp meson ninja # if build with meson
```

### Build with fpm

The [fpm](https://github.com/fortran-lang/fpm) does not support [fypp](https://github.com/aradi/fypp) currently ([#78](https://github.com/fortran-lang/fpm/issues/78)), so in order to use fpm I wrote a simple installation script. If you have fpm, fypp and [fprettify](https://github.com/pseewald/fprettify) (optional) installed this would be the easist way to play with my project. For a quick start, navigate to the source directory and type:

```
./install.sh
```

Further information can be found through `./install.sh --help`.

### Build with meson

[Meson](https://mesonbuild.com/) is a user friendly and fast build tool. To build with Meson:

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
fprettify -i=4 -r src
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
- [x] Review exisiting code, rewrite/refactor code with (1) modern Fortran, (2) functional programming and (3) array-oriented styles.