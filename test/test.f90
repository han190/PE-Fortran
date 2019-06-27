PROGRAM main
  USE euler_utility, ONLY: li, unit_digit, length_of_int, pop, tiny_sp
  IMPLICIT NONE
  !   LOGICAL :: is_greater_than_million
  !   INTEGER :: n = 23, k = 10
  !   REAL :: tmp, rn, rk
  !   tmp = 1.; rn = REAL(n); rk = REAL(k)
  !   DO
  !      PRINT *, rn, rk, tmp
  !      IF (tmp > 1.e6) EXIT
  !      tmp = tmp*rn/rk
  !      rn = rn - 1.
  !      rk = rk - 1.
  !   END DO
  !   IF (tmp > 1.e6) THEN
  !      is_greater_than_million = .TRUE.
  !   ELSE
  !      is_greater_than_million = .FALSE.
  !   END IF
  !   PRINT *, is_greater_than_million

  INTEGER :: i, j, counter = 0
  DO i = 1, 100
     DO j = i, 100
        IF (is_greater_than_million(j, i)) counter = counter + 1
     END DO
  END DO

  !PRINT *, is_greater_than_million(23, 10)
  PRINT *, counter
CONTAINS
  LOGICAL FUNCTION is_greater_than_million(n, k)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: n, k
    REAL :: tmp, rn, rk
    tmp = 1.; rn = REAL(n); rk = REAL(k)
    DO WHILE (rk >= 1.)
       IF (tmp > 1.e6) EXIT
       tmp = tmp*rn/rk
       rn = rn - 1.
       rk = rk - 1.
    END DO
    IF (tmp > 1.e6) THEN
       is_greater_than_million = .TRUE.
    ELSE
       is_greater_than_million = .FALSE.
    END IF
  END FUNCTION is_greater_than_million
END PROGRAM main
