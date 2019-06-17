SUBMODULE(euler_problems) euler_problem_0009
  ! Project Euler: Problem 0009
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0009()
    IMPLICIT NONE
    INTEGER(li) :: a, b, c, n
    n = 1000_li
    outer: DO a = 1_li, n
       inner: DO b = a + 1_li, n
          c = n - a - b
          IF (a**2 + b**2 == c**2) THEN
             WRITE (euler0009, "(i20)") a*b*c
             RETURN
          END IF
       END DO inner
    END DO outer
  END FUNCTION euler0009
END SUBMODULE euler_problem_0009
