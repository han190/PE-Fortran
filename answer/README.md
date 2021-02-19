# Fortran PE Solutions

## Compilers

- Compiler version: GCC version 9.3.0
- Compiler options: -fdiagnostics-color=always -I PE-Fortran.p -I . -I .. -mtune=generic -march=x86-64 -auxbase-strip PE-Fortran.p/src_main_euler_main_m.f90.o -O3 -std=f2018 -ffast-math -J PE-Fortran.p -fpre-include=/usr/include/finclude/math-vector-fortran.h

## Summary

|Benchmarks|Results|
|:----:|:----:|
|Problems solved|  59|
|Time spent|     1.29(s)|
|Time spent per problem|     0.02(s)|

## Relative Difficulty

- Relative Difficulty of a problem =  Normalize [ Tspan / ( Tsum / Nprob ) ]
|Level 0|Level 1|Level 2|Level 3|Level 4|Time<br/>Consuming!|
|:----:|:----:|:----:|:----:|:----:|:----:|
|~10<sup>-6<sup/>|~10<sup>-5<sup/>|~10<sup>-4<sup/>|~10<sup>-3<sup/>|~10<sup>-2<sup/>|~10<sup>-1<sup/>|
||:neutral_face:|:slightly_frowning_face:|:confused:|:frowning_face:|:imp:|

## Answers

|Prob|Answer|Tspan(s)|Relative<br/>Difficulty|
|:----:|:----:|:----:|:----:|
|     1|              233168|  0.000015|:neutral_face:           |
|     2|             4613732|  0.000004|                         |
|     3|                6857|  0.003243|:confused:               |
|     4|              906609|  0.004066|:confused:               |
|     5|           232792560|  0.000002|                         |
|     6|            25164150|  0.000002|                         |
|     7|              104743|  0.002930|:confused:               |
|     8|         23514624000|  0.000071|:slightly_frowning_face: |
|     9|            31875000|  0.000094|:slightly_frowning_face: |
|    10|        142913828922|  0.008232|:frowning_face:          |
|    11|            70600674|  0.000065|:slightly_frowning_face: |
|    12|            76576500|  0.088524|:imp:                    |
|    13|          5537376230|  0.000254|:slightly_frowning_face: |
|    14|              837799|  0.089452|:imp:                    |
|    15|        137846528820|  0.000001|                         |
|    16|                1366|  0.000491|:slightly_frowning_face: |
|    17|               21124|  0.000014|:neutral_face:           |
|    18|                1074|  0.000024|:neutral_face:           |
|    19|                 171|  0.000002|                         |
|    20|                 648|  0.000029|:neutral_face:           |
|    21|               31626|  0.001920|:confused:               |
|    22|           871198282|  0.001723|:confused:               |
|    23|           395465626|  0.000351|:slightly_frowning_face: |
|    24|          2783915460|  0.000004|                         |
|    25|                4782|  0.015360|:frowning_face:          |
|    26|                 983|  0.000393|:slightly_frowning_face: |
|    27|              -59231|  0.000001|                         |
|    28|           669171001|  0.000001|                         |
|    29|                9183|  0.014581|:frowning_face:          |
|    30|              443839|  0.031687|:frowning_face:          |
|    31|               73682|  0.000003|                         |
|    32|               45228|  0.002347|:confused:               |
|    33|                 100|  0.000002|                         |
|    34|               40730|  0.002075|:confused:               |
|    35|                  55|  0.009180|:frowning_face:          |
|    36|              872187|  0.000017|:neutral_face:           |
|    37|              748317|  0.003742|:confused:               |
|    38|           932718654|  0.001220|:confused:               |
|    39|                 840|  0.042026|:frowning_face:          |
|    40|                 210|  0.000002|                         |
|    41|             7652413|  0.054743|:imp:                    |
|    42|                 162|  0.000675|:confused:               |
|    43|         16695334890|  0.071962|:imp:                    |
|    44|             5482660|  0.029265|:frowning_face:          |
|    45|          1533776805|  0.000002|                         |
|    46|                5777|  0.000803|:confused:               |
|    47|              134043|  0.004591|:confused:               |
|    48|          9110846700|  0.016312|:frowning_face:          |
|    49|        296962999629|  0.000033|:neutral_face:           |
|    50|              997651|  0.003376|:confused:               |
|    51|                   x|  0.000000|                         |
|    52|              142857|  0.010195|:frowning_face:          |
|    53|                4075|  0.000007|:neutral_face:           |
|    54|                 376|  0.001784|:confused:               |
|    55|                 249|  0.008420|:frowning_face:          |
|    56|                 972|  0.016729|:frowning_face:          |
|    57|                 153|  0.003350|:confused:               |
|    58|               26241|  0.217258|:imp:                    |
|    59|              129448|  0.017300|:frowning_face:          |
|    60|               26033|  0.505872|:imp:                    |
