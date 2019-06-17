SUBMODULE(euler_problems) euler_problem_0019
  ! Project Euler: Problem 0019
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0019()
    IMPLICIT NONE
    INTEGER(li), DIMENSION(12) :: month_arr
    INTEGER(li) ::  day_of_week, temp, i, j
    month_arr = [31_li, 28_li, 31_li, 30_li, 31_li, 30_li, 31_li, &
         31_li, 30_li, 31_li, 30_li, 31_li]
    temp = 0_li; day_of_week = 1_li
    outer: DO i = 1901_li, 2000_li
       IF (is_leap(i)) month_arr(2) = 29_li
       inner: DO j = 1, SIZE(month_arr)
          day_of_week = day_of_week + MOD(month_arr(j), 7_li) - 1_li
          IF (MOD(day_of_week, 7_li) == 0_li) temp = temp + 1_li
       END DO inner
    END DO outer
    WRITE (euler0019, "(i20)") temp
  END FUNCTION euler0019
  !
  LOGICAL FUNCTION is_leap(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    is_leap = .FALSE.
    IF (((MOD(n, 4) == 0) .AND. (MOD(n, 100) /= 0)) &
         .OR. (MOD(n, 400) == 0)) is_leap = .TRUE.
  END FUNCTION is_leap
END SUBMODULE euler_problem_0019
