SUBMODULE(euler_problems) euler_problem_0050
  ! Project Euler: Problem 0050
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0050()
    INTEGER(ll), PARAMETER :: maxx = 1000000_ll
    INTEGER(ll) :: n, i, j, cur_sum
    INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: p_arr
    CALL prime_array(maxx, p_arr)
    n = 0_ll
    DO
       n = n + 1_ll
       IF (SUM(p_arr(1:n)) > maxx) EXIT
    END DO
    n = n - 1_ll
    j = 1_ll; cur_sum = SUM(p_arr(1:n))
    outer: DO i = N, 1, -1
       j = 1_ll
       cur_sum = SUM(p_arr(1:i))
       inner: DO
          IF (ANY(cur_sum == p_arr)) EXIT outer
          cur_sum = cur_sum - p_arr(j)
          j = j + 1_ll
       END DO inner
    END DO outer
    WRITE (euler0050, "(i20)") cur_sum
  END FUNCTION euler0050
  !
  SUBROUTINE prime_array(n, primes)
    USE euler_utility, ONLY: sieve_of_Eratosthenes
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll), ALLOCATABLE, DIMENSION(:), INTENT(out) :: primes
    INTEGER(ll) :: i, j
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
    CALL sieve_of_Eratosthenes(n, is_prime)
    ALLOCATE (primes(COUNT(is_prime)))
    j = 1_li
    DO i = 2_li, n
       IF (is_prime(i)) THEN
          primes(j) = i
          j = j + 1_li
       END IF
    END DO
  END SUBROUTINE prime_array
END SUBMODULE euler_problem_0050
