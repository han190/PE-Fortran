# Yet another Project Euler solutions...

[Project Euler](https://projecteuler.net/about) is a problem set. I solve PE problems to polish my Fortran skills, expand my math knowledge base and most importantly FOR FUN :smiley:. If you are looking for the same things, check it out! There are two dependencies to this project: [gfortran](https://gcc.gnu.org/wiki/GFortran) and [fypp](https://fypp.readthedocs.io/en/stable/). To install them (assuming you are on Ubuntu and you have [pip](https://pip.pypa.io/en/stable/) installed)

```shell
sudo apt install gfortran
pip install fypp
```

## Compiling and executing this project :hammer:

To compile and execute this project,

```shell
make && make install
```

and the generated `ANSWER.md` will look something like [this](https://github.com/han190/PE-Fortran/tree/master/answer/README.md). To remove all generated files,

```shell
make clean
```

## A todo list :dart:

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example Meson to wrap all the codes. 
  - PS1: Crappy but I did it with Meson.
  - PS2: I rewrite everything using makefile... the good old makefile :zany_face:
- [x] Use a Fortran preprocessor to simplify my code.
- [ ] Write a documentation to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [ ] Add version control.
- [ ] When I finish 75 problems, write a command line interface. When I finish 100 problems (if that is possible), write a GUI.
