SUBMODULE(euler_problems) euler_problem_0049
  ! Project Euler: Problem 0049
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0049()
    USE euler_utility, ONLY: sieve_of_Eratosthenes
    IMPLICIT NONE
    INTEGER(li) :: i, a, b, c
    CHARACTER(4) :: str1, str2, str3
    LOGICAL :: cond
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
    CALL sieve_of_Eratosthenes(10000_li, is_prime)
    DO i = 9973_li, 7661_li, -1_li
       a = i; b = a - 3330_li; c = b - 3330_li
       cond = is_prime(a) .AND. is_prime(b) .AND. is_prime(c)
       IF (.NOT. cond) CYCLE
       CALL sort_int(a); CALL sort_int(b); CALL sort_int(c)
       IF (a == b .AND. a == c) EXIT
    END DO
    WRITE (str1, "(i4)") i
    WRITE (str2, "(i4)") i - 3330_li
    WRITE (str3, "(i4)") i - 6660_li
    euler0049 = str3//str2//str1
  END FUNCTION euler0049
  !
  SUBROUTINE sort_int(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(inout) :: n
    CHARACTER(4) :: str
    WRITE (str, "(i4)") n
    CALL sort_str4(str)
    READ (str, "(i4)") n
  END SUBROUTINE sort_int
  !
  SUBROUTINE sort_str4(str)
    IMPLICIT NONE
    CHARACTER(4), INTENT(inout) :: str
    INTEGER(li) :: i, j
    DO i = 1_li, 4_li
       DO j = i + 1_li, 4_li
          IF (str(i:i) > str(j:j)) CALL swap_str(str(i:i), str(j:j))
       END DO
    END DO
  END SUBROUTINE sort_str4
  !
  SUBROUTINE swap_str(a, b)
    IMPLICIT NONE
    CHARACTER(1), INTENT(inout) :: a, b
    CHARACTER(1) :: tmp
    tmp = a
    a = b
    b = tmp
  END SUBROUTINE swap_str
END SUBMODULE euler_problem_0049
