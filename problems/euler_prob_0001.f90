SUBMODULE(euler_problems) euler_problem_0001
  ! Project Euler: Problem 0001
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0001()
    IMPLICIT NONE
    INTEGER(li) :: i, j, tmp, answer
    i = 3_li; j = 5_li; tmp = 999_li
    answer = sum_divisibly_by(i, tmp) + sum_divisibly_by(j, tmp) &
         & - sum_divisibly_by(i*j, tmp)
    WRITE (euler0001, "(i20)") answer
  END FUNCTION euler0001
  !
  INTEGER(li) FUNCTION sum_divisibly_by(i, j)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: i, j
    INTEGER(li) :: tmp
    tmp = j/i
    sum_divisibly_by = i*(tmp*(tmp + 1_li))/2_li
  END FUNCTION sum_divisibly_by
END SUBMODULE euler_problem_0001
