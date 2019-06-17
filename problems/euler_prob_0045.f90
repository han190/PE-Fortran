SUBMODULE(euler_problems) euler_problem_0045
  ! Project Euler: Problem 0045
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0045()
    IMPLICIT NONE
    INTEGER(ll) :: n, hexagonal_number
    n = 144_ll; hexagonal_number = 41328_ll
    DO
       IF (is_pentagonal(hexagonal_number)) EXIT
       hexagonal_number = hexagonal_number + 4_ll*n + 1_ll
       n = n + 1_ll
    END DO
    WRITE (euler0045, "(i20)") hexagonal_number
  END FUNCTION euler0045
  !
  LOGICAL FUNCTION is_pentagonal(P)
    USE euler_utility, ONLY: is_integer
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: P
    IF (p <= 0) THEN
       ERROR STOP "IS_PENTAGONAL: Input number has to be positive."
    END IF
    ASSOCIATE (x=>SQRT(24.0_dp*REAL(P, dp) + 1.0_dp))
      IF (is_integer(x) .AND. MOD(INT(x, ll), 6_ll) == 5_ll) THEN
         is_pentagonal = .TRUE.
      ELSE
         is_pentagonal = .FALSE.
      END IF
    END ASSOCIATE
  END FUNCTION is_pentagonal
END SUBMODULE euler_problem_0045
