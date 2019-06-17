SUBMODULE(euler_problems) euler_problem_0021
  ! Project Euler: Problem 0021
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0021()
    IMPLICIT NONE
    INTEGER(li) :: p, t
    t = 0_li; 
    DO p = 1_li, 10000_li
       IF (sum_of_proper_divisors(sum_of_proper_divisors(p)) == p &
            .AND. p /= sum_of_proper_divisors(p)) t = t + p
    END DO
    WRITE (euler0021, "(i20)") t
  END FUNCTION euler0021
  !
  INTEGER(li) FUNCTION sum_of_proper_divisors(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li) :: sum, i
    sum = 1_li; 
    DO i = 2_li, INT(SQRT(REAL(n, sp)), li)
       IF (MOD(n, i) == 0_li) sum = sum + i + n/i
    END DO
    sum_of_proper_divisors = sum
  END FUNCTION sum_of_proper_divisors
END SUBMODULE euler_problem_0021
