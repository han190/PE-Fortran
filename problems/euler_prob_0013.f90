SUBMODULE(euler_problems) euler_problem_0013
  ! Project Euler: Problem 0013
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0013()
    IMPLICIT NONE
    CHARACTER(len=20) :: file_name
    INTEGER(ll), DIMENSION(50, 100) :: long_int
    INTEGER(ll), DIMENSION(50) :: temp
    INTEGER(ll) :: i, j, k, t
    file_name = 'euler0013.txt'
    OPEN (unit=3, file=file_name, status='old', action='read')
    DO i = 1, 100
       READ (3, "(50(i1))") long_int(1_ll:50_ll, i)
    END DO
    CLOSE (3)
    DO j = 50_ll, 1_ll, -1_ll
       temp(j) = 0_ll
       DO i = 1_ll, 100_ll
          temp(j) = temp(j) + long_int(j, i)
       END DO
    END DO
    DO j = 50_ll, 2_ll, -1_ll
       t = temp(j)
       temp(j) = temp(j) - (temp(j)/10_ll)*10_ll
       temp(j - 1_ll) = temp(j - 1_ll) + (t - temp(j))/10_ll
    END DO
    k = FLOOR(LOG10(REAL(temp(1))) + 1_ll)
    t = 0_ll
    DO i = 1_ll, 10_ll - k
       t = t + temp(i)*10_ll**(11_ll - k - i)
    END DO
    WRITE (euler0013, "(i20)") t
  END FUNCTION euler0013
END SUBMODULE euler_problem_0013
