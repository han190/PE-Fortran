SUBMODULE(euler_problems) euler_problem_0010
  ! Project Euler: Problem 0010
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0010()
    !USE euler_utility, ONLY: is_prime
    USE euler_utility, ONLY: sieve_of_Eratosthenes
    IMPLICIT NONE
    INTEGER(ll) :: i, temp
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
    temp = 0_ll
    CALL sieve_of_Eratosthenes(2000000_ll, is_prime)
    DO i = 2_ll, 2000000_ll
       IF (is_prime(i)) THEN
          temp = temp + i
       END IF
    END DO
    WRITE (euler0010, "(i20)") temp
  END FUNCTION euler0010
END SUBMODULE euler_problem_0010
