SUBMODULE(euler_problems) euler_problem_0004
  ! Project Euler: Problem 0004
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0004()
    USE euler_utility, ONLY: is_palindromic
    IMPLICIT NONE
    INTEGER(li) :: a, b, p_max
    p_max = 0_li; a = 100_li
    DO WHILE (a <= 999_li)
       b = 100_li
       DO WHILE (b <= 999_li)
          IF (is_palindromic(a*b) .AND. a*b > p_max) THEN
             p_max = a*b
          END IF
          b = b + 1_li
       END DO
       a = a + 1_li
    END DO
    WRITE (euler0004, "(i20)") p_max
  END FUNCTION euler0004
END SUBMODULE euler_problem_0004
