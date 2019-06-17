SUBMODULE(euler_problems) euler_problem_0047
  ! Project Euler: Problem 0047
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0047()
    IMPLICIT NONE
    INTEGER(li), PARAMETER :: maxx = 1000000_li
    INTEGER(li), DIMENSION(maxx) :: n_factor
    INTEGER(li), DIMENSION(4) :: goal
    INTEGER(li) :: i, j
    n_factor = 0_li
    DO i = 2, maxx - 1
       IF (n_factor(i) == 0_li) THEN
          DO j = 2_li*i, maxx - 1_li, i
             n_factor(j) = n_factor(j) + 1_li
          END DO
       END IF
    END DO
    goal = [4_li, 4_li, 4_li, 4_li]
    DO i = 2_li, maxx - 1_li
       IF (ALL(n_factor(i:i + 3_li) == goal(:))) EXIT
    END DO
    WRITE (euler0047, "(i20)") i
  END FUNCTION euler0047
END SUBMODULE euler_problem_0047
