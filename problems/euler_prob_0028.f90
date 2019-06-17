SUBMODULE(euler_problems) euler_problem_0028
  ! Project Euler: Problem 0028
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0028()
    IMPLICIT NONE
    INTEGER(li) :: i, k, ur, bl, br, ul, temp
    temp = 1_li
    DO i = 2_li, 501_li
       k = 2_li*i - 1_li
       ur = k**2_li; bl = ur - 2_li*k + 2_li
       br = bl - (k - 1_li); ul = bl + (k - 1_li)
       temp = temp + ur + bl + br + ul
    END DO
    WRITE (euler0028, "(i20)") temp
  END FUNCTION euler0028
END SUBMODULE euler_problem_0028
