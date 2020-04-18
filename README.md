# Project Euler with Modern Fortran

[Project Euler](https://projecteuler.net/about) is a weekly updated math problem set. I created this project mainly because I want to practice my Fortran programming skills, so although not necessary, everything is written by modern Fortran. If you are learning Fortran, this project might be helpful.

## Dependencies

You will need a Fortran compiler to run this project. The [GNU Fortran compiler](https://gcc.gnu.org/fortran/) is recommended. The code is tested with gfortran (version >= 9.0, since the Fortran 2008 feature [findloc](https://gcc.gnu.org/onlinedocs/gfortran/FINDLOC.html) is used in problem 54) on Mac and Linux (Fedora). Also, the [Meson](https://mesonbuild.com/index.html) Build system (>= 0.53) is used to compile this project. For Fedora users

```shell
sudo dnf install gfortran meson ninja
```

and for Mac users, install [brew](https://brew.sh/) first and then

```shell
sudo brew install gfortran meson ninja
```

## Compiling and executing the program

To build, compile and run the project,

```shell
meson build && cd build && ninja && ./euler
```

## A summary of results

Since it is aimed as a language learning project, the performance of the code is NOT a priority. The `Tspan` for each problem in the output file `ANSWER.md` indicate the time it takes to call the corresponding problem function and `probs` is a procedure pointer array.

```fortran
do i = 1, nop
    call cpu_time(t_i)
    ans(i) = probs(i)%euler_prob_p()
    call cpu_time(t_f)
    tspan(i) = t_f - t_i
end do
```

|Specs of my computer                                           |
|:--------------------------------------------------------------|
|Compiler: GNU Fortran (GCC) 9.2.1 20190827 (Red Hat 9.2.1-1)   |
|CPU: Intel Core i7-8700K @ 12x 4.7GHz                          |
|RAM: 2526MiB / 31962MiB                                        |


### Answers and Benchmarks

|Prob  |Answer              |Tspan(s)  |T/Ttot(%) |
|:-----|:-------------------|:---------|:--------:|
|     1|              233168|  0.000009|   0.0016%|
|     2|             4613732|  0.000003|   0.0005%|
|     3|                6857|  0.004011|   0.6913%|
|     4|              906609|  0.003923|   0.6761%|
|     5|           232792560|  0.000004|   0.0007%|
|     6|            25164150|  0.000001|   0.0002%|
|     7|              104743|  0.002963|   0.5107%|
|     8|         23514624000|  0.000086|   0.0148%|
|     9|            31875000|  0.000094|   0.0162%|
|    10|        142913828922|  0.012693|   2.1876%|
|    11|            70600674|  0.000079|   0.0136%|
|    12|            76576500|  0.088699|  15.2872%|
|    13|          5537376230|  0.000275|   0.0474%|
|    14|              837799|  0.093739|  16.1558%|
|    15|        137846528820|  0.000004|   0.0007%|
|    16|                1366|  0.000498|   0.0858%|
|    17|               21124|  0.000014|   0.0024%|
|    18|                1074|  0.000036|   0.0062%|
|    19|                 171|  0.000002|   0.0003%|
|    20|                 648|  0.000030|   0.0052%|
|    21|               31626|  0.002049|   0.3531%|
|    22|           871198282|  0.001610|   0.2775%|
|    23|             4179871|  0.010238|   1.7645%|
|    24|          2783915460|  0.000004|   0.0007%|
|    25|                4782|  0.015413|   2.6564%|
|    26|                 983|  0.000394|   0.0679%|
|    27|              -59231|  0.000001|   0.0002%|
|    28|           669171001|  0.000001|   0.0002%|
|    29|                9183|  0.021295|   3.6702%|
|    30|              443839|  0.031156|   5.3697%|
|    31|               73682|  0.000003|   0.0005%|
|    32|               45228|  0.002374|   0.4092%|
|    33|                 100|  0.000001|   0.0002%|
|    34|               40730|  0.002041|   0.3518%|
|    35|                  55|  0.008936|   1.5401%|
|    36|              872187|  0.000017|   0.0029%|
|    37|              748317|  0.003691|   0.6361%|
|    38|           932718654|  0.001239|   0.2135%|
|    39|                 840|  0.041978|   7.2349%|
|    40|                 210|  0.000003|   0.0005%|
|    41|             7652413|  0.066460|  11.4543%|
|    42|                 162|  0.000422|   0.0727%|
|    43|         16695334890|  0.070200|  12.0989%|
|    44|             5482660|  0.029039|   5.0048%|
|    45|          1533776805|  0.000002|   0.0003%|
|    46|                5777|  0.000781|   0.1346%|
|    47|              134043|  0.004720|   0.8135%|
|    48|          9110846700|  0.016781|   2.8922%|
|    49|        296962999629|  0.000031|   0.0053%|
|    50|              997651|  0.003376|   0.5819%|
|    51|                   x|  0.000001|   0.0002%|
|    52|              142857|  0.010042|   1.7307%|
|    53|                4075|  0.000006|   0.0010%|
|    54|                 376|  0.001576|   0.2716%|
|    55|                 249|  0.008429|   1.4527%|
|    56|                 972|  0.015471|   2.6664%|
|    57|                 153|  0.003275|   0.5644%|
|    58|                   x|  0.000000|   0.0000%|

| Summary                        |               |
|:-------------------------------|:--------------|
| Problems solved                |   56          |
| Total time spent               |   0.580219(s) |
| Average time spent per problem |   0.010361(s) |


## A todo list

Here is what I plan to do in the future. (Not likely to be done recently cuz I am kinda busy...)

- [x] Organize folders, use Shell scripts to compile codes.
- [x] Use a build tool, for example [Meson](https://mesonbuild.com/) to wrap all the codes. (Crappy but I did it...)
- [ ] Write a wiki page to clearly explain the algorithms, Fortran features, or anything that is interesting for each question.
- [ ] When I finish 75 problems, write a command line interface. When I finish 100 problems, write a GUI.
