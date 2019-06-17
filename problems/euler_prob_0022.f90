SUBMODULE(euler_problems) euler_problem_0022
  ! Project Euler: Problem 0022
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0022()
    USE euler_utility, ONLY: lexical_sort
    CHARACTER(len=20) :: file_name
    CHARACTER(len=20), DIMENSION(6000) :: array_of_names
    INTEGER :: stat, i
    INTEGER(ll) :: temp
    array_of_names = 'n/a'
    file_name = 'euler0022.txt'
    OPEN (unit=6, file=file_name, status='old', action='read')
    READ (6, *, iostat=stat) array_of_names(1:6000)
    i = 1
    DO WHILE (array_of_names(i) /= 'n/a')
       i = i + 1
    END DO
    CALL lexical_sort(array_of_names(1:i - 1))
    temp = 0_ll
    DO i = 1, i - 1
       temp = temp + i*score_of_letters(array_of_names(i))
    END DO
    WRITE (euler0022, "(i20)") temp
  END FUNCTION euler0022
  !
  INTEGER(ll) FUNCTION score_of_letters(str)
    IMPLICIT NONE
    CHARACTER(*), INTENT(in) :: str
    INTEGER(ll) :: j, sum
    sum = 0_ll
    DO j = 1_ll, len_TRIM(str)
       sum = sum + IACHAR(str(j:j)) - 64_ll
    END DO
    score_of_letters = sum
  END FUNCTION score_of_letters
END SUBMODULE euler_problem_0022
