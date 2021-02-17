# Fortran PE Solutions

## Compilers

- Compiler version: GCC version 9.3.0
- Compiler options: -fdiagnostics-color=always -I PE-Fortran.p -I . -I .. -mtune=generic -march=x86-64 -auxbase-strip PE-Fortran.p/src_main_euler_main.f90.o -O3 -std=f2018 -ffast-math -J PE-Fortran.p -fpre-include=/usr/include/finclude/math-vector-fortran.h

## Summary

|Benchmarks|Results|
|:----:|:----:|
|Problems solved|  59|
|Time spent|     1.32(s)|
|Time spent per problem|     0.02(s)|

## Relative Difficulty

Relative Difficulty of a problem =  Normalize [ Tspan / ( Tsum / Nprob ) ]
(Thus RD is a real number between 0 and 1.)
|Level 0|Level 1|Level 2|Level 3|Level 4|Time<br/>Consuming!|
|:----:|:----:|:----:|:----:|:----:|:----:|
|~10<sup>-6<sup/>|~10<sup>-5<sup/>|~10<sup>-4<sup/>|~10<sup>-3<sup/>|~10<sup>-2<sup/>|~10<sup>-1<sup/>|
||:neutral_face:|:slightly_frowning_face:|:confused:|:frowning_face:|:imp:|

## Answers

|Prob|Answer|Tspan(s)|Relative<br/>Difficulty|
|:----:|:----:|:----:|:----:|
|     1|              233168|  0.000016|:neutral_face:           |
|     2|             4613732|  0.000004|                         |
|     3|                6857|  0.002811|:confused:               |
|     4|              906609|  0.003991|:confused:               |
|     5|           232792560|  0.000003|                         |
|     6|            25164150|  0.000003|                         |
|     7|              104743|  0.002941|:confused:               |
|     8|         23514624000|  0.000069|:slightly_frowning_face: |
|     9|            31875000|  0.000095|:slightly_frowning_face: |
|    10|        142913828922|  0.006866|:frowning_face:          |
|    11|            70600674|  0.000067|:slightly_frowning_face: |
|    12|            76576500|  0.088830|:imp:                    |
|    13|          5537376230|  0.000241|:slightly_frowning_face: |
|    14|              837799|  0.132367|:imp:                    |
|    15|        137846528820|  0.000003|                         |
|    16|                1366|  0.000499|:confused:               |
|    17|               21124|  0.000014|:neutral_face:           |
|    18|                1074|  0.000027|:neutral_face:           |
|    19|                 171|  0.000002|                         |
|    20|                 648|  0.000029|:neutral_face:           |
|    21|               31626|  0.002041|:confused:               |
|    22|           871198282|  0.001618|:confused:               |
|    23|           395465626|  0.000377|:slightly_frowning_face: |
|    24|          2783915460|  0.000004|                         |
|    25|                4782|  0.015405|:frowning_face:          |
|    26|                 983|  0.000394|:slightly_frowning_face: |
|    27|              -59231|  0.000001|                         |
|    28|           669171001|  0.000001|                         |
|    29|                9183|  0.013994|:frowning_face:          |
|    30|              443839|  0.031727|:frowning_face:          |
|    31|               73682|  0.000003|                         |
|    32|               45228|  0.002370|:confused:               |
|    33|                 100|  0.000001|                         |
|    34|               40730|  0.002064|:confused:               |
|    35|                  55|  0.008648|:frowning_face:          |
|    36|              872187|  0.000025|:neutral_face:           |
|    37|              748317|  0.003915|:confused:               |
|    38|           932718654|  0.001431|:confused:               |
|    39|                 840|  0.042104|:frowning_face:          |
|    40|                 210|  0.000003|                         |
|    41|             7652413|  0.046581|:frowning_face:          |
|    42|                 162|  0.000747|:confused:               |
|    43|         16695334890|  0.078775|:imp:                    |
|    44|             5482660|  0.029108|:frowning_face:          |
|    45|          1533776805|  0.000002|                         |
|    46|                5777|  0.001278|:confused:               |
|    47|              134043|  0.004281|:confused:               |
|    48|          9110846700|  0.016478|:frowning_face:          |
|    49|        296962999629|  0.000035|:neutral_face:           |
|    50|              997651|  0.003491|:confused:               |
|    51|                   x|  0.000000|                         |
|    52|              142857|  0.010439|:frowning_face:          |
|    53|                4075|  0.000008|:neutral_face:           |
|    54|                 376|  0.001768|:confused:               |
|    55|                 249|  0.009597|:frowning_face:          |
|    56|                 972|  0.016802|:frowning_face:          |
|    57|                 153|  0.003457|:confused:               |
|    58|               26241|  0.217733|:imp:                    |
|    59|              129448|  0.017327|:frowning_face:          |
|    60|               26033|  0.494287|:imp:                    |
