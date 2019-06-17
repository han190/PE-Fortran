SUBMODULE(euler_problems) euler_problem_0027
  ! Project Euler: Problem 0027
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0027()
    IMPLICIT NONE
    INTEGER(li) :: a, b
    a = -(2_li*30_li + 1_li)
    b = quadratic_primes(1_li, 41_li, 30_li)
    WRITE (euler0027, "(i20)") a*b
  END FUNCTION euler0027
  !
  INTEGER(li) FUNCTION quadratic_primes(i, j, n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: i, j, n
    IF (ABS(i) >= 1000_li .OR. ABS(j) > 1000_li) THEN
       WRITE (*, *) "Coefficient a or b is out of range. &
            & Answer is set to be zero."
       quadratic_primes = 0_li
       RETURN
    END IF
    quadratic_primes = n**2 + i*n + j
  END FUNCTION quadratic_primes
END SUBMODULE euler_problem_0027
