SUBMODULE(euler_utility) is_prime_module
  IMPLICIT NONE
CONTAINS
  !
  MODULE LOGICAL FUNCTION is_prime_li(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li) :: limit, i
    is_prime_li = .FALSE.
    limit = INT(SQRT(REAL(n, sp)) + 1_li)
    IF (n <= 1_li) THEN
       is_prime_li = .FALSE.
    ELSE IF (n <= 3_li) THEN
       is_prime_li = .TRUE.
    ELSE IF (MOD(n, 2_li) == 0_li) THEN
       is_prime_li = .FALSE.
    ELSE
       loop_1: DO i = 3_li, limit, 2_li
          IF (MOD(n, i) == 0_li) THEN
             is_prime_li = .FALSE.
             EXIT loop_1
          ELSE
             is_prime_li = .TRUE.
          END IF
       END DO loop_1
    END IF
  END FUNCTION is_prime_li
  !
  MODULE LOGICAL FUNCTION is_prime_ll(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll) :: limit, i
    is_prime_ll = .FALSE.
    limit = INT(SQRT(REAL(n, sp)) + 1_ll)
    IF (n <= 1_ll) THEN
       is_prime_ll = .FALSE.
    ELSE IF (n <= 3_ll) THEN
       is_prime_ll = .TRUE.
    ELSE IF (MOD(n, 2_ll) == 0_ll) THEN
       is_prime_ll = .FALSE.
    ELSE
       loop_1: DO i = 3_ll, limit, 2_ll
          IF (MOD(n, i) == 0_ll) THEN
             is_prime_ll = .FALSE.
             EXIT loop_1
          ELSE
             is_prime_ll = .TRUE.
          END IF
       END DO loop_1
    END IF
  END FUNCTION is_prime_ll
  !
  MODULE SUBROUTINE sieve_of_Eratosthenes_li(n, is_prime)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
    INTEGER(li) :: i, j
    ALLOCATE (is_prime(2:n))
    is_prime = .TRUE.
    DO i = 2_ll, FLOOR(SQRT(REAL(n, dp)), li)
       IF (is_prime(i)) THEN
          j = i*i
          DO WHILE (j <= n)
             is_prime(j) = .FALSE.
             j = j + i
          END DO
       END IF
    END DO
  END SUBROUTINE sieve_of_Eratosthenes_li
  !
  MODULE SUBROUTINE sieve_of_Eratosthenes_ll(n, is_prime)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
    INTEGER(ll) :: i, j
    ALLOCATE (is_prime(2:n))
    is_prime = .TRUE.
    DO i = 2_ll, FLOOR(SQRT(REAL(n, dp)), ll)
       IF (is_prime(i)) THEN
          j = i*i
          DO WHILE (j <= n)
             is_prime(j) = .FALSE.
             j = j + i
          END DO
       END IF
    END DO
  END SUBROUTINE sieve_of_Eratosthenes_ll
END SUBMODULE is_prime_module
