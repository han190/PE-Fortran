SUBMODULE(euler_problems) euler_problem_0041
  ! Project Euler: Problem 0041
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0041()
    USE euler_utility, ONLY: is_prime, is_pandigital, length_of_int
    IMPLICIT NONE
    INTEGER(li) :: i
    ! The 9 and 8 pandigital number is always divisible by 3.
    DO i = 7654321_li, 1_li, -1_li
       IF (is_prime(i) .AND. is_pandigital(i, length_of_int(i))) EXIT
    END DO
    WRITE (euler0041, "(i20)") i
  END FUNCTION euler0041
END SUBMODULE euler_problem_0041
