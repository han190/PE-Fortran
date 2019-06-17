SUBMODULE(euler_problems) euler_problem_0034
  ! Project Euler: Problem 0034
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0034()
    IMPLICIT NONE
    INTEGER(li) :: i, sum
    sum = 0_li
    DO i = 1_li, 40585_li
       IF (is_curious(i)) sum = sum + i
    END DO
    WRITE (euler0034, "(i20)") sum
  END FUNCTION euler0034
  !
  LOGICAL FUNCTION is_curious(n)
    USE euler_utility, ONLY: int_2_arr, factorial
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: array
    INTEGER(li) :: i1, temp
    IF (n == 1_li .OR. n == 2_li) THEN
       is_curious = .FALSE.
       RETURN
    END IF
    temp = 0_li; is_curious = .FALSE.
    CALL int_2_arr(n, array)
    DO i1 = 1_li, SIZE(array)
       temp = temp + factorial(array(i1))
    END DO
    IF (temp == n) is_curious = .TRUE.
  END FUNCTION is_curious
END SUBMODULE euler_problem_0034
