SUBMODULE(euler_problems) euler_problem_0017
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0017()
    IMPLICIT NONE
    INTEGER(li) :: total_count, i
    total_count = 0_li
    DO i = 1_li, 9_li
       total_count = total_count + count_letters(i)
    END DO
    DO i = 10_li, 19_li
       total_count = total_count + count_letters10(i)
    END DO
    DO i = 20_li, 99_li
       total_count = total_count + count_letters10(i)
    END DO
    DO i = 100_li, 999_li
       total_count = total_count + count_letters100(i)
    END DO
    WRITE (euler0017, "(i20)") total_count + 11_li
  END FUNCTION euler0017
  !
  INTEGER(li) FUNCTION count_letters(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li) :: temp
    SELECT CASE (n)
    CASE (0_li)
       temp = 0_li
    CASE (1_li, 2_li, 6_li, 10_li)
       temp = 3_li
    CASE (4_li, 5_li, 9_li)
       temp = 4_li
    CASE (3_li, 7_li, 8_li, 40_li, 50_li, 60_li)
       temp = 5_li
    CASE (11_li, 12_li, 20_li, 30_li, 80_li, 90_li)
       temp = 6_li
    CASE (15_li, 16_li, 70_li)
       temp = 7_li
    CASE (13_li, 14_li, 18_li, 19)
       temp = 8_li
    CASE (17_li)
       temp = 9_li
    CASE (1000_li)
       temp = 8_li
    CASE default
       temp = 1000_li
    END SELECT
    count_letters = temp
  END FUNCTION count_letters
  !
  INTEGER(li) FUNCTION count_letters10(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li) :: d1, d2
    d1 = 0_li; d2 = 0_li
    IF (count_letters(n) /= 1000_li) THEN
       count_letters10 = count_letters(n)
    ELSE
       d1 = (n/10_li)*10_li
       d2 = n - d1
       count_letters10 = count_letters(d1) + count_letters(d2)
    END IF
    IF (n == 0_li) THEN
       count_letters10 = 0_li
    END IF
  END FUNCTION count_letters10
  !
  INTEGER(li) FUNCTION count_letters100(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li) :: d1, d2, hun, and
    hun = 7_li; and = 3_li
    IF (MOD(n, 100_li) == 0_li) THEN
       d1 = n/100_li
       count_letters100 = count_letters(d1) + hun
    ELSE
       d1 = n/100_li
       d2 = n - (n/100_li)*100_li
       count_letters100 = count_letters(d1) + hun + and + &
            count_letters10(d2)
    END IF
  END FUNCTION count_letters100
END SUBMODULE euler_problem_0017
