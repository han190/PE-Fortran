SUBMODULE(euler_problems) euler_problem_0038
  ! Project Euler: Problem 0038
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0038()
    USE euler_utility, ONLY: is_pandigital, length_of_int, append
    IMPLICIT NONE
    INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: common_array
    INTEGER(ll) :: i, j, temp
    outer: DO i = 2_ll, 10000_ll
       j = 1_ll; temp = 0_ll
       inner: DO
          IF (length_of_int(temp) > 9) CYCLE outer
          IF (is_pandigital(temp)) THEN
             CALL append(common_array, temp)
          END IF
          temp = i*j + temp*10_ll**length_of_int(i*j)
          j = j + 1_ll
       END DO inner
    END DO outer
    WRITE (euler0038, "(i20)") MAXVAL(common_array, dim=1)
  END FUNCTION euler0038
END SUBMODULE euler_problem_0038
