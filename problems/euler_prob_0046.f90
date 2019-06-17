SUBMODULE(euler_problems) euler_problem_0046
  ! Project Euler: Problem 0046
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0046()
    USE euler_utility, ONLY: sieve_of_Eratosthenes
    IMPLICIT NONE
    INTEGER(li), PARAMETER :: n = 10000_li
    INTEGER(li) :: j, res = 1_li
    LOGICAL :: not_found = .TRUE.
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
    CALL sieve_of_Eratosthenes(n, is_prime)
    outer: DO WHILE (not_found)
       res = res + 2_li
       j = 2_li
       not_found = .FALSE.
       inner: DO WHILE (res >= j)
          IF (is_prime(j) .AND. is_twice_square(res - j)) THEN
             not_found = .TRUE.
             EXIT inner
          END IF
          j = j + 1_li
       END DO inner
    END DO outer
    WRITE (euler0046, "(i20)") res
  END FUNCTION euler0046
  !
  LOGICAL FUNCTION is_twice_square(n)
    USE euler_utility, ONLY: tiny_dp
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    REAL(dp) :: sqrt_nover2
    is_twice_square = .FALSE.
    sqrt_nover2 = SQRT(0.5_dp*REAL(n, dp))
    IF (sqrt_nover2 - REAL(FLOOR(sqrt_nover2, li), dp) < tiny_dp) THEN
       is_twice_square = .TRUE.
    ELSE
       is_twice_square = .FALSE.
    END IF
  END FUNCTION is_twice_square
END SUBMODULE euler_problem_0046
