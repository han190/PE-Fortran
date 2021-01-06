# Yet another Project Euler solutions...

[Project Euler](https://projecteuler.net/about) is a problem set. I solve PE problems to polish my Fortran skills, expand my math  knowledge base and most importantly, for fun. If you are looking for the same things, check it out! The project is tested with [`gfortran`](https://gcc.gnu.org/fortran/) (version >= 9.0), [`ifort`](https://software.intel.com/content/www/us/en/develop/articles/intel-oneapi-fortran-compiler-release-notes.html) and [`flang`](https://releases.llvm.org/11.0.0/tools/flang/docs/ReleaseNotes.html). Also, [fypp](https://github.com/aradi/fypp) is used as the Fortran preprocessor. To install fypp the easist way is 

```shell
pip install fypp
```

## Compiling and executing the program

To compile and execute the project,

```shell
./PEFortran.sh
```

the answers are saved in `ANSWER.md`. For further information, type `./PEFortran -h`, 

```shell
./PEFortran -h
Project Euler with Modern Fortran
Version: 0.0.1
Flags possible:
  -b=, --build=       Build options: optimize/debug
  -c=, --compiler=    Compiler options: gfortran/ifort/flang
  -n=, --num_prob=    Number of problems: (max=60)
  -d,  --default      This implies:
                      --build=optimize --compiler=gfortran
                      --num_prob=60
  -v,  --version      Check version.
  -r,  --remove       Remove build files and ANSWER.md.
  -h,  --help         Pop out this message.
```

## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example Meson to wrap all the codes. (PS1: Crappy but I did it...) (PS2: I deleted it since it is not necessary to use a build tool with such a small project.)
- [x] Use some Fortran preprocessor to simplify my code.
- [ ] Write a wiki page to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [ ] When I finish 75 problems, write a command line interface. When I finish 100 problems (if that is possible), write a GUI.
