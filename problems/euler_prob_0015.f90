SUBMODULE(euler_problems) euler_problem_0015
  ! Project Euler: Problem 0015
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0015()
    IMPLICIT NONE
    INTEGER(ll) :: i, temp
    temp = 1_ll
    DO i = 1_ll, 20_ll
       temp = temp*(20_ll + i)/i
    END DO
    WRITE (euler0015, "(i20)") temp
  END FUNCTION euler0015
END SUBMODULE euler_problem_0015
