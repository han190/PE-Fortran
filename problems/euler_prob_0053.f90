SUBMODULE(euler_problems) euler_problem_0053
  ! Project Euler: Problem 0052
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0053()
    IMPLICIT NONE
    INTEGER :: i, j, counter = 0
    DO i = 1, 100
       DO j = i, 100
          IF (is_greater_than_million(j, i)) counter = counter + 1
       END DO
    END DO
    WRITE (euler0053, "(i20)") counter
  END FUNCTION euler0053

  LOGICAL FUNCTION is_greater_than_million(n, k)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: n, k
    REAL :: tmp, rn, rk
    tmp = 1.; rn = REAL(n); rk = REAL(k)
    DO WHILE (rk >= 1.)
       IF (tmp > 1.e6) EXIT
       tmp = tmp*rn/rk
       rn = rn - 1.
       rk = rk - 1.
    END DO
    IF (tmp > 1.e6) THEN
       is_greater_than_million = .TRUE.
    ELSE
       is_greater_than_million = .FALSE.
    END IF
  END FUNCTION is_greater_than_million
END SUBMODULE euler_problem_0053
