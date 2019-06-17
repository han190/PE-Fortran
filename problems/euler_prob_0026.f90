SUBMODULE(euler_problems) euler_problem_0026
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0026()
    USE euler_utility, ONLY: sieve_of_Eratosthenes
    IMPLICIT NONE
    INTEGER(ll) :: temp, k, ki, i
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
    CALL sieve_of_Eratosthenes(1000_ll, is_prime)
    temp = 0_ll; k = 1_ll
    DO i = 7_ll, 1000_ll
       IF (is_prime(i)) THEN
          temp = multiplicative_order(i, 10_ll)
          IF (temp > k) THEN
             k = temp
             ki = i
          END IF
       END IF
    END DO
    WRITE (euler0026, "(i20)") ki
  END FUNCTION euler0026
  !
  INTEGER(ll) FUNCTION multiplicative_order(n, m)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n, m
    INTEGER(ll) :: res, k1, temp1
    k1 = 1_ll; temp1 = 1_ll
    DO
       res = MOD(k1*m, n)
       IF (res /= 1_ll) THEN
          k1 = res
          temp1 = temp1 + 1_ll
       ELSE
          EXIT
       END IF
    END DO
    multiplicative_order = temp1
  END FUNCTION multiplicative_order
END SUBMODULE euler_problem_0026
