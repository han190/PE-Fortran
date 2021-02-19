# Fortran PE Solutions

## Compilers

- Compiler version: GCC version 9.3.0
- Compiler options: -fdiagnostics-color=always -I PE-Fortran.p -I . -I .. -mtune=generic -march=x86-64 -auxbase-strip PE-Fortran.p/src_main_euler_main_m.f90.o -O3 -std=f2018 -ffast-math -J PE-Fortran.p -fpre-include=/usr/include/finclude/math-vector-fortran.h

## Summary

|Benchmarks|Results|
|:----:|:----:|
|Problems solved|  59|
|Time spent|     1.50(s)|
|Time spent per problem|     0.03(s)|

## Relative Difficulty

- Relative Difficulty of a problem =  Normalize [ Tspan / ( Tsum / Nprob ) ]

|Level 0|Level 1|Level 2|Level 3|Level 4|Time<br/>Consuming!|
|:----:|:----:|:----:|:----:|:----:|:----:|
|~10<sup>-6<sup/>|~10<sup>-5<sup/>|~10<sup>-4<sup/>|~10<sup>-3<sup/>|~10<sup>-2<sup/>|~10<sup>-1<sup/>|
||:neutral_face:|:slightly_frowning_face:|:confused:|:frowning_face:|:imp:|

## Answers

|Prob|Answer|Tspan(s)|Relative<br/>Difficulty|
|:----:|:----:|:----:|:----:|
|     1|              233168|  0.000014|:neutral_face:           |
|     2|             4613732|  0.000003|                         |
|     3|                6857|  0.002978|:confused:               |
|     4|              906609|  0.004144|:confused:               |
|     5|           232792560|  0.000003|                         |
|     6|            25164150|  0.000004|                         |
|     7|              104743|  0.002943|:confused:               |
|     8|         23514624000|  0.000085|:slightly_frowning_face: |
|     9|            31875000|  0.000094|:slightly_frowning_face: |
|    10|        142913828922|  0.006868|:frowning_face:          |
|    11|            70600674|  0.000073|:slightly_frowning_face: |
|    12|            76576500|  0.089292|:imp:                    |
|    13|          5537376230|  0.000344|:slightly_frowning_face: |
|    14|              837799|  0.133004|:imp:                    |
|    15|        137846528820|  0.000005|                         |
|    16|                1366|  0.000490|:slightly_frowning_face: |
|    17|               21124|  0.000015|:neutral_face:           |
|    18|                1074|  0.000035|:neutral_face:           |
|    19|                 171|  0.000002|                         |
|    20|                 648|  0.000030|:neutral_face:           |
|    21|               31626|  0.001948|:confused:               |
|    22|           871198282|  0.001645|:confused:               |
|    23|                   1|  0.164368|:imp:                    |
|    24|          2783915460|  0.000007|:neutral_face:           |
|    25|                4782|  0.015515|:frowning_face:          |
|    26|                 983|  0.000395|:slightly_frowning_face: |
|    27|              -59231|  0.000001|                         |
|    28|           669171001|  0.000001|                         |
|    29|                9183|  0.014077|:frowning_face:          |
|    30|              443839|  0.032004|:frowning_face:          |
|    31|               73682|  0.000004|                         |
|    32|               45228|  0.002378|:confused:               |
|    33|                 100|  0.000002|                         |
|    34|               40730|  0.002126|:confused:               |
|    35|                  55|  0.008410|:frowning_face:          |
|    36|              872187|  0.000025|:neutral_face:           |
|    37|              748317|  0.003887|:confused:               |
|    38|           932718654|  0.001446|:confused:               |
|    39|                 840|  0.042248|:frowning_face:          |
|    40|                 210|  0.000004|                         |
|    41|             7652413|  0.049893|:frowning_face:          |
|    42|                 162|  0.000753|:confused:               |
|    43|         16695334890|  0.082230|:imp:                    |
|    44|             5482660|  0.029197|:frowning_face:          |
|    45|          1533776805|  0.000004|                         |
|    46|                5777|  0.001371|:confused:               |
|    47|              134043|  0.004281|:confused:               |
|    48|          9110846700|  0.016556|:frowning_face:          |
|    49|        296962999629|  0.000036|:neutral_face:           |
|    50|              997651|  0.003440|:confused:               |
|    51|                   x|  0.000001|                         |
|    52|              142857|  0.010361|:frowning_face:          |
|    53|                4075|  0.000009|:neutral_face:           |
|    54|                 376|  0.001830|:confused:               |
|    55|                 249|  0.009750|:frowning_face:          |
|    56|                 972|  0.016990|:frowning_face:          |
|    57|                 153|  0.003502|:confused:               |
|    58|               26241|  0.218852|:imp:                    |
|    59|              129448|  0.017371|:frowning_face:          |
|    60|               26033|  0.503943|:imp:                    |
