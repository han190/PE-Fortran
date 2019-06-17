SUBMODULE(euler_problems) euler_problem_0042
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0042()
    IMPLICIT NONE
    CHARACTER(len=20) :: file_name
    CHARACTER(len=20), DIMENSION(2000) :: names
    INTEGER(li) :: stat, i, j
    names = 'n/a'
    file_name = 'euler0042.txt'
    OPEN (unit=7, file=file_name, status='old', action='read')
    READ (7, *, iostat=stat) names(1:2000)
    i = 1; j = 0_li
    DO WHILE (names(i) /= 'n/a')
       IF (is_triangular_number(score_of_letters(names(i)))) THEN
          j = j + 1_li
       END IF
       i = i + 1
    END DO
    WRITE (euler0042, "(i20)") j
  END FUNCTION euler0042
  !
  INTEGER(li) FUNCTION score_of_letters(str)
    IMPLICIT NONE
    CHARACTER(*), INTENT(in) :: str
    INTEGER(li) :: j, sum
    sum = 0_li
    DO j = 1_li, len_TRIM(str)
       sum = sum + IACHAR(str(j:j)) - 64_li
    END DO
    score_of_letters = sum
  END FUNCTION score_of_letters
  !
  LOGICAL FUNCTION is_triangular_number(t)
    USE euler_utility, ONLY: is_integer
    INTEGER(li), INTENT(in) :: t
    IF (is_integer(0.5_sp*(SQRT(8.0_sp*REAL(t, sp) + 1.0_sp) - 1.0_sp))) THEN
       is_triangular_number = .TRUE.
    ELSE
       is_triangular_number = .FALSE.
    END IF
  END FUNCTION is_triangular_number
END SUBMODULE euler_problem_0042
