SUBMODULE(euler_problems) euler_problem_0005
  ! Project Euler: Problem 0005
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0005()
    USE euler_utility, ONLY: lcm
    IMPLICIT NONE
    INTEGER(ll) :: n, i, temp
    temp = 1_ll; n = 20_ll
    DO i = 1_ll, n
       temp = lcm(temp, i)
    END DO
    WRITE (euler0005, "(i20)") temp
  END FUNCTION euler0005
END SUBMODULE euler_problem_0005
