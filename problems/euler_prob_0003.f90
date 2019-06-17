SUBMODULE(euler_problems) euler_problem_0003
  ! Project Euler: Problem 0003
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0003()
    USE euler_utility, ONLY: sieve_of_Eratosthenes
    IMPLICIT NONE
    INTEGER(ll) :: n, fmax, i
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
    n = 600851475143_ll
    fmax = INT(SQRT(REAL(n, sp)) + 1.0_sp, ll)
    CALL sieve_of_Eratosthenes(fmax, is_prime)
    DO i = fmax, 2_ll, -1_ll
       IF (is_prime(i) .AND. (MOD(n, i) == 0_ll)) EXIT
    END DO
    WRITE (euler0003, "(i20)") i
  END FUNCTION euler0003
END SUBMODULE euler_problem_0003
