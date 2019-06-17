SUBMODULE(euler_problems) euler_problem_0014
  ! Project Euler: Problem 0014
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0014()
    IMPLICIT NONE
    INTEGER(ll) :: k, i, temp
    temp = 0_ll; k = 0_ll
    DO i = 500000_ll, 1000000_ll
       IF (collatz(i) > temp) THEN
          temp = collatz(i)
          k = i
       END IF
    END DO
    WRITE (euler0014, "(i20)") k
  END FUNCTION euler0014
  !
  INTEGER(ll) FUNCTION collatz(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll) :: t, m
    t = 0_ll; m = n
    loop_1: DO
       IF (m == 1) EXIT loop_1
       IF (MOD(m, 2_ll) == 0_ll) THEN
          m = m/2_ll
       ELSE
          m = 3_ll*m + 1_ll
       END IF
       t = t + 1_ll
    END DO loop_1
    collatz = t
  END FUNCTION collatz
END SUBMODULE euler_problem_0014
