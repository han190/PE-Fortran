SUBMODULE(euler_problems) euler_problem_0039
  ! Project Euler: Problem 0039
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0039()
    IMPLICIT NONE
    INTEGER(li) :: i, j, k
    INTEGER(li), DIMENSION(1000) :: count_array
    count_array = 0_li
    DO i = 1_li, 500_li
       DO j = 1_li, i - 1_li
          IF (sqrt_is_integer(i**2 + j**2)) THEN
             k = INT(SQRT(REAL(i**2 + j**2, sp)), li)
             IF (i + j + k <= 1000_li) THEN
                count_array(i + j + k) = count_array(i + j + k) + 1_li
             END IF
          END IF
       END DO
    END DO
    WRITE (euler0039, "(i20)") MAXLOC(count_array, dim=1)
  END FUNCTION euler0039
  !
  LOGICAL FUNCTION sqrt_is_integer(n)
    USE euler_utility, ONLY: is_integer
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    IF (is_integer(SQRT(REAL(n, sp)))) THEN
       sqrt_is_integer = .TRUE.
    ELSE
       sqrt_is_integer = .FALSE.
    END IF
  END FUNCTION sqrt_is_integer
END SUBMODULE euler_problem_0039
