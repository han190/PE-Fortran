SUBMODULE(euler_problems) euler_problem_0033
  ! Project Euler: Problem 0033
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0033()
    USE euler_utility, ONLY: gcd
    IMPLICIT NONE
    INTEGER(li) :: c, d, n, dp, np
    c = 1_li; d = 1_li; n = 1_li; np = 1_li; dp = 1_li
    DO c = 1_li, 9_li
       DO d = 1_li, c - 1_li
          DO n = 1_li, d - 1_li
             IF ((n*10_li + c)*d == (c*10_li + d)*n) THEN
                np = np*n; dp = dp*d
             END IF
          END DO
       END DO
    END DO
    WRITE (euler0033, "(i20)") dp/gcd(np, dp)
  END FUNCTION euler0033
END SUBMODULE euler_problem_0033
