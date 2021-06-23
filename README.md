# Yet another Project Euler solutions...

[Project Euler](https://projecteuler.net/about) is a problem set. I solve PE problems to polish my Fortran skills, expand my math knowledge base and most importantly FOR FUN. If you are looking for the same things, check it out!

## Getting Started

### Prerequistes

You will need [Meson](https://mesonbuild.com/index.html), [Ninja](https://ninja-build.org/manual.html), and [gfortran](https://gcc.gnu.org/wiki/GFortran) or [ifort](https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top.html) to compile and execute this project. To install them (assuming you are on Ubuntu and have [pip](https://pip.pypa.io/en/stable/) installed)

```bash
sudo apt install gfortran
pip install meson ninja
# For Windows users, I highly recommend choco!
# With choco installed, run powershell as admin and 'choco install mingw'
```

For a minimum installation of the Intel Fortran compiler, take a look at [this discussion](https://fortran-lang.discourse.group/t/intel-releases-oneapi-toolkit-free-fortran-2018/471/35?u=han190)!

### Build with Meson

To build this project with Meson, navigate to the root directory of this repo and type

```bash
meson --prefix=$(realpath ./bin/) build
# meson --prefix="$(Resolve-Path .)\bin" build # Powershell
meson install -C build
```

To build with `ifort`

```bash
FC=ifort meson --prefix=$(realpath ./bin/) build
```

### Build with [fpm](https://github.com/fortran-lang/fpm)

To build this project with fpm (recomended!), navigate to the root directory of this repo and

```bash
fpm build # --profile release
fpm run PE-Fortran -- -a 61 -d $(realpath ./data/)
```

## Usage

Navigate to `bin`, and type `./PE-Fortran --help` 

```
PE Fortran Solution
Arguments available:
   -v, or --version                    Version.
   -a N, or --all N                    Compute problem 1 to N.
   -n N, or --problem-number N         Compute problem N.
   -d /path/to/data/, or 
   --data-directory /path/to/data/     Path to data.
   -h, --help                          Pop up this message.

Usage:
(1) Compute problem 1 to 50:
   ./PE-Fortran -a 50 -d /path/to/data/
(2) Compute problem 50:
   ./PE-Fortran -n 50 -d /path/to/data/

Tips:
 *  Some of the problems require extra data, you can
    find all the data in the directory: 
    /path/to/the/cloned/PE-Fortran/data/
 *  You can use relative path by
    ./PE-Fortran -n 50 -d $(realpath /relative/path/to/data/)
```

For example, to calculate the first 50 problems

```bash
./PE-Fortran --all 50 --data-directory $(realpath .)
# ./PE-Fortran -a 50 -d $(realpath .) # The short way
# .\PE-Fortran.exe --all 50 --data-directory $(Resolve-Path .) # Powershell
```

The generated `answer.md` will look something like [this](https://github.com/han190/PE-Fortran/tree/master/answer/answer.md).

## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example Meson to wrap all the codes. 
  - PS: Crappy but I did it with Meson.
- [x] Use a Fortran preprocessor to simplify my code.
- [ ] Write a documentation to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [x] Add version control.
- [ ] When I finish 75 problems, write a command line interface. When I finish 100 problems (if that is possible), write a GUI.
