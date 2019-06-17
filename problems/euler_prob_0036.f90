SUBMODULE(euler_problems) euler_problem_0036
  ! Project Euler: Problem 0036
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0036()
    IMPLICIT NONE
    INTEGER(ll), PARAMETER :: limit = 1000000_ll
    INTEGER(ll) :: sum, i, next_ten_pow, p
    sum = 0_ll; i = 1_ll; next_ten_pow = 10_ll
    p = gen_single_palindormic_10(i)
    DO WHILE (p <= limit)
       IF (is_palindromic_2(p)) sum = sum + p
       p = p + (i - i/10_ll)*next_ten_pow
       IF (p <= limit .AND. is_palindromic_2(p)) sum = sum + p
       i = i + 1_ll
       IF (i == next_ten_pow) next_ten_pow = next_ten_pow*10_ll
       p = gen_single_palindormic_10(i)
    END DO
    WRITE (euler0036, "(i20)") sum
  END FUNCTION euler0036
  !
  LOGICAL FUNCTION is_palindromic_2(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll) :: a, b, digit
    a = n; b = 0_ll
    DO WHILE (a > 0_ll)
       digit = MOD(a, 2_ll)
       b = b*2_ll + digit; a = a/2_ll
    END DO
    IF (n == b) THEN
       is_palindromic_2 = .TRUE.
    ELSE
       is_palindromic_2 = .FALSE.
    END IF
  END FUNCTION is_palindromic_2
  !
  INTEGER(ll) FUNCTION gen_single_palindormic_10(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll) :: temp, i1
    temp = n; i1 = n/10_ll
    DO WHILE (i1 > 0_ll)
       temp = temp*10_ll + MOD(i1, 10_ll)
       i1 = i1/10_ll
    END DO
    gen_single_palindormic_10 = temp
  END FUNCTION gen_single_palindormic_10
END SUBMODULE euler_problem_0036
