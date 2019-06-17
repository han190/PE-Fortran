SUBMODULE(euler_problems) euler_problem_0016
  ! Project Euler: Problem 0016
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0016()
    USE euler_utility, ONLY: carry
    IMPLICIT NONE
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: iarr
    INTEGER(li) :: l, i, j
    l = FLOOR(1000.0_sp*LOG10(2.0_sp) + 1.0_sp)
    ALLOCATE (iarr(l))
    iarr = [(0_li, i=1_li, l)]; iarr(l) = 1_li
    DO i = 1_li, 1000_li
       iarr(l) = iarr(l)*2_li
       DO j = l, 2_li, -1_li
          CALL carry(iarr(j - 1:j), 2_li)
       END DO
    END DO
    WRITE (euler0016, "(i20)") SUM(iarr)
  END FUNCTION euler0016
END SUBMODULE euler_problem_0016
