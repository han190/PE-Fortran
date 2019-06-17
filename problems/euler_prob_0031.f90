SUBMODULE(euler_problems) euler_problem_0031
  ! Project Euler: Problem 0031
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0031()
    IMPLICIT NONE
    INTEGER(li), DIMENSION(0:7) :: coins
    INTEGER(li), DIMENSION(0:7, 0:200) :: ans_arr
    INTEGER(li) :: i, j
    coins = [1_li, 2_li, 5_li, 10_li, 20_li, 50_li, 100_li, 200_li]
    ans_arr(0, :) = 1_li
    outer: DO j = 0, 200
       inner: DO i = 1, 7
          ans_arr(i, j) = 0_li
          ASSOCIATE (next=>ans_arr(i, j), prev=>ans_arr(i - 1, j))
            IF (j < coins(i)) THEN
               next = prev
            ELSE
               next = prev + ans_arr(i, j - coins(i))
            END IF
          END ASSOCIATE
       END DO inner
    END DO outer
    WRITE (euler0031, "(i20)") ans_arr(7, 200)
  END FUNCTION euler0031
END SUBMODULE euler_problem_0031
