# Fortran PE Solutions

## Compilers

- Compiler version: Intel(R) Fortran Intel(R) 64 Compiler Classic for applications running on Intel(R) 64, Version 2021.1 Build 20201112_000000
- Compiler options: -IPE-Fortran.p -I. -I.. -D_FILE_OFFSET_BITS=64 -stand=f18 -O3 -xHost -ipo -module PE-Fortran.p -gen-dep=PE-Fortran.p/src_main_euler_main_m.f90.o -gen-depformat=make -o PE-Fortran.p/src_main_euler_main_m.f90.o -c

## Summary

|Benchmarks|Results|
|:----:|:----:|
|Problems solved|  59|
|Time spent|     1.27(s)|
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
|     1|              233168|  0.000011|:neutral_face:           |
|     2|             4613732|  0.000005|:neutral_face:           |
|     3|                6857|  0.003396|:confused:               |
|     4|              906609|  0.004896|:frowning_face:          |
|     5|           232792560|  0.000002|                         |
|     6|            25164150|  0.000002|                         |
|     7|              104743|  0.002894|:confused:               |
|     8|         23514624000|  0.000074|:slightly_frowning_face: |
|     9|            31875000|  0.000149|:slightly_frowning_face: |
|    10|        142913828922|  0.005695|:frowning_face:          |
|    11|            70600674|  0.000050|:slightly_frowning_face: |
|    12|            76576500|  0.027900|:frowning_face:          |
|    13|          5537376230|  0.000187|:slightly_frowning_face: |
|    14|              837799|  0.093779|:imp:                    |
|    15|        137846528820|  0.000002|                         |
|    16|                1366|  0.000496|:confused:               |
|    17|               21124|  0.000015|:neutral_face:           |
|    18|                1074|  0.000042|:slightly_frowning_face: |
|    19|                 171|  0.000004|                         |
|    20|                 648|  0.000029|:neutral_face:           |
|    21|               31626|  0.002354|:confused:               |
|    22|           474822712|  0.001680|:confused:               |
|    23|             4179871|  0.121727|:imp:                    |
|    24|          2783915460|  0.000004|                         |
|    25|                4782|  0.018522|:frowning_face:          |
|    26|                 983|  0.000394|:confused:               |
|    27|              -59231|  0.000001|                         |
|    28|           669171001|  0.000002|                         |
|    29|                9183|  0.022060|:frowning_face:          |
|    30|              443839|  0.042749|:imp:                    |
|    31|               73682|  0.000003|                         |
|    32|               45228|  0.002097|:confused:               |
|    33|                 100|  0.000002|                         |
|    34|               40730|  0.002423|:confused:               |
|    35|                  55|  0.005649|:frowning_face:          |
|    36|              872187|  0.000031|:neutral_face:           |
|    37|              748317|  0.012455|:frowning_face:          |
|    38|           932718654|  0.001946|:confused:               |
|    39|                 840|  0.016338|:frowning_face:          |
|    40|                 210|  0.000002|                         |
|    41|             7652413|  0.046685|:imp:                    |
|    42|                 162|  0.000460|:confused:               |
|    43|         16695334890|  0.051596|:imp:                    |
|    44|             5482660|  0.046120|:imp:                    |
|    45|          1533776805|  0.000001|                         |
|    46|                5777|  0.002130|:confused:               |
|    47|              134043|  0.004064|:frowning_face:          |
|    48|          9110846700|  0.014477|:frowning_face:          |
|    49|        296962999629|  0.000022|:neutral_face:           |
|    50|              997651|  0.003741|:confused:               |
|    51|                   x|  0.000001|                         |
|    52|              142857|  0.012211|:frowning_face:          |
|    53|                4075|  0.000007|:neutral_face:           |
|    54|                 376|  0.000856|:confused:               |
|    55|                 249|  0.044408|:imp:                    |
|    56|                 972|  0.028177|:frowning_face:          |
|    57|                 153|  0.003492|:confused:               |
|    58|               26241|  0.227061|:imp:                    |
|    59|              129448|  0.014654|:frowning_face:          |
|    60|               26033|  0.383319|:imp:                    |
