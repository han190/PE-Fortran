SUBMODULE(euler_problems) euler_problem_0007
  ! Project Euler: Problem 0007
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0007()
    USE euler_utility, ONLY: is_prime
    IMPLICIT NONE
    INTEGER(li) :: i, j
    i = 0_li; j = 0_li
    loop_1: DO
       IF (j == 10001_li) EXIT loop_1
       i = i + 1_li
       IF (MOD(i, 2_li) == 2_li .AND. i /= 2_li) THEN
          CYCLE loop_1
       ELSE IF (is_prime(INT(i, ll))) THEN
          j = j + 1_li
       END IF
    END DO loop_1
    WRITE (euler0007, "(i20)") i
  END FUNCTION euler0007
END SUBMODULE euler_problem_0007
