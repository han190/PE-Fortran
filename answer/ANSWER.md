# Fortran PE Solutions

## Compilers

- Compiler version: Intel(R) Fortran Intel(R) 64 Compiler Classic for applications running on Intel(R) 64, Version 2021.2.0 Build 20210228_000000
- Compiler options: -IPE-Fortran.p -I. -I.. -D_FILE_OFFSET_BITS=64 -warn general -warn truncated_source -stand=f18 -O3 -xHost -ipo -module PE-Fortran.p -gen-dep=PE-Fortran.p/src_euler_main_m.f90.o -gen-depformat=make -o PE-Fortran.p/src_euler_main_m.f90.o -c

## Summary

|Benchmarks|Results|
|:----:|:----:|
|Problems solved|  61|
|Time spent|     1.26(s)|
|Time spent per problem|     0.02(s)|

## Relative Difficulty

- Relative Difficulty(Prob) =  Normalize(Tspan(Prob) / ( Ttot/Nprob ))

where Normalize(x<sub>i</sub>) =  (x<sub>i</sub> - min(x))/(max(x) - min(x))

|Level 0|Level 1|Level 2|Level 3|Level 4|Level 5|
|:----:|:----:|:----:|:----:|:----:|:----:|
|~10<sup>-6</sup>|~10<sup>-5</sup>|~10<sup>-4</sup>|~10<sup>-3</sup>|~10<sup>-2</sup>|~10<sup>-1</sup>|
||_Lv1_|_Lv2_|_Lv3_|_Lv4_|_Time-consuming_|

## Answers

|Prob|Answer|Tspan(s)|Relative Difficulty|
|:----:|:----:|:----:|:----:|
|     1|              233168|  0.000010|                    _Lv1_|
|     2|             4613732|  0.000006|                    _Lv1_|
|     3|                6857|  0.003161|                    _Lv3_|
|     4|              906609|  0.005294|                    _Lv4_|
|     5|           232792560|  0.000002|                         |
|     6|            25164150|  0.000002|                         |
|     7|              104743|  0.003333|                    _Lv3_|
|     8|         23514624000|  0.000075|                    _Lv2_|
|     9|            31875000|  0.000161|                    _Lv2_|
|    10|        142913828922|  0.005513|                    _Lv4_|
|    11|            70600674|  0.000054|                    _Lv2_|
|    12|            76576500|  0.030822|                    _Lv4_|
|    13|          5537376230|  0.000320|                    _Lv2_|
|    14|              837799|  0.106472|         _Time-consuming_|
|    15|        137846528820|  0.000002|                         |
|    16|                1366|  0.000572|                    _Lv3_|
|    17|               21124|  0.000023|                    _Lv1_|
|    18|                1074|  0.000041|                    _Lv1_|
|    19|                 171|  0.000003|                         |
|    20|                 648|  0.000031|                    _Lv1_|
|    21|               31626|  0.002567|                    _Lv3_|
|    22|           474822712|  0.001954|                    _Lv3_|
|    23|             4179871|  0.022338|                    _Lv4_|
|    24|          2783915460|  0.000005|                         |
|    25|                4782|  0.020163|                    _Lv4_|
|    26|                 983|  0.000459|                    _Lv3_|
|    27|              -59231|  0.000001|                         |
|    28|           669171001|  0.000001|                         |
|    29|                9183|  0.022188|                    _Lv4_|
|    30|              443839|  0.046725|         _Time-consuming_|
|    31|               73682|  0.000004|                         |
|    32|               45228|  0.002374|                    _Lv3_|
|    33|                 100|  0.000002|                         |
|    34|               40730|  0.002639|                    _Lv3_|
|    35|                  55|  0.005715|                    _Lv4_|
|    36|              872187|  0.000041|                    _Lv1_|
|    37|              748317|  0.015589|                    _Lv4_|
|    38|           932718654|  0.002287|                    _Lv3_|
|    39|                 840|  0.017993|                    _Lv4_|
|    40|                 210|  0.000002|                         |
|    41|             7652413|  0.046698|         _Time-consuming_|
|    42|                 162|  0.000507|                    _Lv3_|
|    43|         16695334890|  0.057221|         _Time-consuming_|
|    44|             5482660|  0.048633|         _Time-consuming_|
|    45|          1533776805|  0.000001|                         |
|    46|                5777|  0.002581|                    _Lv3_|
|    47|              134043|  0.004217|                    _Lv3_|
|    48|          9110846700|  0.015598|                    _Lv4_|
|    49|        296962999629|  0.000023|                    _Lv1_|
|    50|              997651|  0.003233|                    _Lv3_|
|    51|              121313|  0.003581|                    _Lv3_|
|    52|              142857|  0.013885|                    _Lv4_|
|    53|                4075|  0.000008|                    _Lv1_|
|    54|                 376|  0.000935|                    _Lv3_|
|    55|                 249|  0.047746|         _Time-consuming_|
|    56|                 972|  0.003510|                    _Lv3_|
|    57|                 153|  0.003826|                    _Lv3_|
|    58|               26241|  0.247564|         _Time-consuming_|
|    59|              129448|  0.016212|                    _Lv4_|
|    60|               26033|  0.423314|         _Time-consuming_|
|    61|               28684|  0.001951|                    _Lv3_|
