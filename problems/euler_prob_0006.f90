SUBMODULE(euler_problems) euler_problem_0006
  ! Project Euler: Problem 0006
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0006()
    IMPLICIT NONE
    INTEGER(li) :: n, i, sum_square, square_sum
    sum_square = 0_li; square_sum = 0_li; n = 100_li
    DO i = 1_li, n
       sum_square = sum_square + i**2
    END DO
    square_sum = ((1_li + n)*n/2)**2
    WRITE (euler0006, "(i20)") square_sum - sum_square
  END FUNCTION euler0006
END SUBMODULE euler_problem_0006
