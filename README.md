# Yet another Project Euler solutions...

[Project Euler](https://projecteuler.net/about) is a problem set. I solve PE problems to polish my Fortran skills, expand my math knowledge base and most importantly FOR FUN. If you are looking for the same things, check it out!

## Getting Started

### Prerequistes

You will need a Fortran compiler ([gfortran](https://gcc.gnu.org/wiki/GFortran) or [ifort](https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top.html)) and the [Fortran Package Manager (fpm)](https://github.com/fortran-lang/fpm) to compile/run/test/install this project. 

For a minimum installation of the Intel Fortran compiler, take a look at [this discussion](https://fortran-lang.discourse.group/t/intel-releases-oneapi-toolkit-free-fortran-2018/471/35?u=han190)!

### Build with fpm

To build this project with fpm, navigate to the root directory of this repo and

```bash
fpm build
```

and run the project with

```bash
fpm run -- -a 50
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

## Quick start

```bash
conda activate fpm
module load compiler
fpm build --profile release --compiler ifort
fpm install --profile release --compiler ifort

PE-Fortran --all 63 --fancy
# -------------------------- --------------------
# PE Fortran Solutions
# Problems solved/tried:                0062/0063
# Total time spent (s):                      1.37
# Time spent/problem (s):                    0.02

# Spec of my PC:
#  OS: Ubuntu 20.04 focal
#  Kernel: x86_64 Linux 5.4.72-microsoft-standard-WSL2
#  Uptime: 2h 4m
#  Packages: 1556
#  Shell: zsh 5.8
#  Resolution: 1920x1200
#  WM: Weston WM
#  GTK Theme: Adwaita [GTK3]
#  Disk: 875G / 3.5T (25%)
#  CPU: Intel Core i7-8700K @ 12x 3.696GHz
#  RAM: 764MiB / 32040MiB
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