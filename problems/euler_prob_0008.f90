SUBMODULE(euler_problems) euler_problem_0008
  ! Project Euler: Problem 0008
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0008()
    IMPLICIT NONE
    CHARACTER(len=20) :: file_name
    INTEGER(ll), DIMENSION(1000_ll) :: long_int
    INTEGER(ll) :: i, s, temp
    file_name = 'euler0008.txt'
    OPEN (unit=1, file=file_name, status='old', action='read')
    DO i = 1_ll, 20_ll
       s = (i - 1_ll)*50_ll + 1_ll
       READ (1, "(50(i1))") long_int(s:s + 49_ll)
    END DO
    CLOSE (1)
    temp = 0_ll
    DO i = 1_ll, 988_ll
       IF (PRODUCT(long_int(i:i + 12_ll)) > temp) THEN
          temp = PRODUCT(long_int(i:i + 12_ll))
       END IF
    END DO
    WRITE (euler0008, "(i20)") temp
  END FUNCTION euler0008
END SUBMODULE euler_problem_0008
