SUBMODULE(euler_problems) euler_problem_0020
  ! Project Euler: Problem 0020
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0020()
    USE euler_utility, ONLY: carry
    IMPLICIT NONE
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: int_arr
    INTEGER(li) :: l, i, j
    REAL(sp) :: lr
    lr = 0.0_sp
    DO i = 1_li, 100_li
       lr = lr + LOG10(REAL(i, sp))
    END DO
    l = FLOOR(lr + 1_li, li)
    ALLOCATE (int_arr(l))
    int_arr = [(0_li, i=1_li, l)]; int_arr(l) = 1_li
    DO i = 1_li, 100_li
       int_arr(l) = int_arr(l)*i
       DO j = l, 2_li, -1_li
          CALL carry(int_arr(j - 1:j), i)
       END DO
    END DO
    WRITE (euler0020, "(i20)") SUM(int_arr)
  END FUNCTION euler0020
END SUBMODULE euler_problem_0020
