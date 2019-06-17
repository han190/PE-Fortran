SUBMODULE(euler_problems) euler_problem_0012
  ! Project Euler: Problem 0012
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0012()
    IMPLICIT NONE
    INTEGER(li) :: j, k
    j = 0_li; k = 0_li
    DO
       j = j + 1_li; k = k + j
       IF (nums_of_divs(k) > 500_li) EXIT
    END DO
    WRITE (euler0012, "(i20)") k
  END FUNCTION euler0012
  !
  INTEGER(li) FUNCTION nums_of_divs(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li) :: m, temp, i
    m = INT(SQRT(REAL(n, sp))); temp = 0_li
    DO i = 1_li, m - 1_li
       IF (MOD(n, i) == 0_li) temp = temp + 1_li
    END DO
    temp = temp*2_li
    IF (MOD(n, m) == 0_li) temp = temp + 1_li
    nums_of_divs = temp
  END FUNCTION nums_of_divs
END SUBMODULE euler_problem_0012
