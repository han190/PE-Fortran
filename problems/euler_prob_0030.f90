SUBMODULE(euler_problems) euler_problem_0030
  ! Project Euler: Problem 0030
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0030()
    USE euler_utility, ONLY: int_2_arr
    IMPLICIT NONE
    INTEGER(li) :: temp, i
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: arr
    temp = 0_li
    DO i = 2_li, 999999_li
       CALL int_2_arr(i, arr)
       IF (SUM(arr**5_li) == i) THEN
          temp = temp + i
       END IF
    END DO
    WRITE (euler0030, "(i20)") temp
  END FUNCTION euler0030
END SUBMODULE euler_problem_0030
