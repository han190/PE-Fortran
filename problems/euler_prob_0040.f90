SUBMODULE(euler_problems) euler_problem_0040
  ! Project Euler: Problem 0040
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0040()
    USE euler_utility, ONLY: int_2_arr
    IMPLICIT NONE
    INTEGER(li), DIMENSION(:), ALLOCATABLE :: arr
    INTEGER(li), DIMENSION(6) :: dig_arr, d
    INTEGER(li) :: i, s
    dig_arr = [0_li, 9_li, 189_li, 2889_li, 38889_li, 488889_li]
    d = [(1_li, i=1, 6)]
    DO i = 2_li, 6_li
       ASSOCIATE (x=>FLOOR(REAL((10_li**i - dig_arr(i))/i, sp), li))
         d(i) = x + 10_li**(i - 1_li) - 1_li
       END ASSOCIATE
       CALL int_2_arr(d(i), arr)
       s = MOD(10_li**i - dig_arr(i), i)
       d(i) = arr(s)
       DEALLOCATE (arr)
    END DO
    WRITE (euler0040, "(i20)") PRODUCT(d)
  END FUNCTION euler0040
END SUBMODULE euler_problem_0040
