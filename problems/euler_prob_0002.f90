SUBMODULE(euler_problems) euler_problem_0002
  ! Project Euler: Problem 0002
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0002()
    USE euler_utility, ONLY: fibonacci
    IMPLICIT NONE
    INTEGER(li) :: i, sum
    i = 2_li
    sum = 0_li
    DO WHILE (fibonacci(i) <= 4000000_li)
       sum = sum + fibonacci(i)
       i = i + 3_li
    END DO
    WRITE (euler0002, "(i20)") sum
  END FUNCTION euler0002
END SUBMODULE euler_problem_0002
