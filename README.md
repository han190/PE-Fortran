# Project Euler with Modern Fortran

[Project Euler](https://projecteuler.net/about) is a weekly updated math problem set. I created this project mainly because I want to practice my Fortran programming skills, so although not necessary, everything is written by modern Fortran. If you are learning Fortran, this project might be helpful.

You will need a Fortran compiler to run this project. The [GNU Fortran compiler](https://gcc.gnu.org/fortran/) is recommended. The code is tested with `gfortran` (version >= 9.0, since the Fortran 2008 feature [findloc](https://gcc.gnu.org/onlinedocs/gfortran/FINDLOC.html) is used in problem 54) and `ifort`. Also, this project uses [fypp](https://github.com/aradi/fypp) as the preprocessor. To install fypp the easist way is 

```shell
pip install fypp
```

To install `gfortran`

```shell
sudo apt install gfortran
```

## Compiling and executing the program

To build the project,

```shell
./PEFortran.sh
```

the answers can be found in `ANSWER.md`. Other flags available can found by `./PEFortran -h`, 

```shell
./PEFortran -h
Project Euler with Modern Fortran
Version: 0.0.1
Flags possible:
  -b=, --build=       Build options: release/debug
  -c=, --compiler=    Compiler options: gfortran/ifort
  -n=, --num_prob=    Number of problems: (max=60)
  -d,  --default      This implies:
                      --build=release --compiler=gfortran
                      --num_prob=60(currently solved)
  -v,  --version      Check version.
  -r,  --remove       Remove build files and ANSWER.md.
  -h,  --help         To pop out this dialog.
```


## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example [Meson](https://mesonbuild.com/) to wrap all the codes. (PS1: Crappy but I did it...) (PS2: I deleted it since it is not necessary to use a build tool with such a small project.)
- [x] Use some Fortran preprocessor to simplify my code.
- [ ] Write a wiki page to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [ ] When I finish 75 problems, write a command line interface. When I finish 100 problems (if that is possible), write a GUI.
