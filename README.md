# Yet another Project Euler solutions...

[Project Euler](https://projecteuler.net/about) is a problem set. I solve PE problems to polish my Fortran skills, expand my math knowledge base and most importantly FOR FUN :smiley:. If you are looking for the same things, check it out!

## Getting Started

### Prerequistes

You will need [Meson](https://mesonbuild.com/index.html), [Ninja](https://ninja-build.org/manual.html), [fypp](https://fypp.readthedocs.io/en/stable/), and [gfortran](https://gcc.gnu.org/wiki/GFortran) or [ifort](https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top.html) to compile and execute this project. To install them (assuming you are on Ubuntu and have [pip](https://pip.pypa.io/en/stable/) installed)

```shell
sudo apt install gfortran
pip install fypp meson ninja
```

For a minimum installation of the Intel Fortran compiler, take a look at [this discussion](https://fortran-lang.discourse.group/t/intel-releases-oneapi-toolkit-free-fortran-2018/471/35?u=han190)!

### Build with Meson

To build this project with Meson, navigate to the root directory of this repo and type

```shell
meson builddir
meson test -C builddir
```

To build with `ifort`

```shell
FC=ifort meson builddir
```

The generated `builddir/ANSWER.md` will look something like [this](https://github.com/han190/PE-Fortran/tree/master/answer/README.md).

## A todo list :dart:

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example Meson to wrap all the codes. 
  - PS: Crappy but I did it with Meson :zany_face:.
- [x] Use a Fortran preprocessor to simplify my code.
- [ ] Write a documentation to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [ ] Add version control.
- [ ] When I finish 75 problems, write a command line interface. When I finish 100 problems (if that is possible), write a GUI.
