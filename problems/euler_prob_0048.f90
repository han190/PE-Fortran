SUBMODULE(euler_problems) euler_problem_0048
  ! Project Euler: Problem 0048
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0048()
    IMPLICIT NONE
    INTEGER(li), DIMENSION(10) :: sum
    INTEGER(li) :: i
    sum = 0_li
    DO i = 1_li, 1000_li
       sum = add10(sum, self_pow10(i))
    END DO
    WRITE (euler0048, "(20(i1))") sum
  END FUNCTION euler0048
  !
  FUNCTION add10(a, b) RESULT(tmp)
    USE euler_utility, ONLY: carry, unit_digit
    IMPLICIT NONE
    INTEGER(li), DIMENSION(10), INTENT(in) :: a, b
    INTEGER(li) :: j
    INTEGER(li), DIMENSION(10) :: tmp
    tmp = a + b
    DO j = 10_li, 2_li, -1_li
       CALL carry(tmp(j - 1:j))
    END DO
    tmp(1) = unit_digit(tmp(1))
  END FUNCTION add10
  !
  FUNCTION self_pow10(n) RESULT(iarr)
    USE euler_utility, ONLY: carry, unit_digit
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li), DIMENSION(10) :: iarr
    INTEGER(li) :: i, j
    iarr = 0_li; iarr(10) = n
    DO i = 1_li, n - 1_li
       iarr(10) = iarr(10)*n
       DO j = 10_li, 2_li, -1_li
          CALL carry(iarr(j - 1:j), n)
       END DO
       iarr(1) = unit_digit(iarr(1))
    END DO
  END FUNCTION self_pow10
END SUBMODULE euler_problem_0048
