SUBMODULE(euler_problems) euler_problem_0032
  ! Project Euler: Problem 0032
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0032()
    USE euler_utility, ONLY: length_of_int, is_pandigital, remove_duplicates
    IMPLICIT NONE
    INTEGER(ll) :: temp, i, j, k
    INTEGER(ll), DIMENSION(9) :: array_of_products
    INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: array_of_non_repeats
    k = 1_ll
    DO i = 1_ll, 9_ll
       DO j = 1234_ll, 9876_ll
          IF (length_of_int(i*j) > 4) CYCLE
          temp = i*10_ll**8 + j*10_ll**4 + i*j
          IF (is_pandigital(temp, length_of_int(temp))) THEN
             array_of_products(k) = i*j; k = k + 1_ll
          END IF
       END DO
    END DO
    DO i = 12_ll, 98_ll
       DO j = 123_ll, 987_ll
          IF (length_of_int(i*j) > 4) CYCLE
          temp = i*10_ll**7 + j*10_ll**4 + i*j
          IF (is_pandigital(temp, length_of_int(temp))) THEN
             array_of_products(k) = i*j; k = k + 1_ll
          END IF
       END DO
    END DO
    CALL remove_duplicates(array_of_products, array_of_non_repeats)
    WRITE (euler0032, "(i20)") SUM(array_of_non_repeats)
  END FUNCTION euler0032
END SUBMODULE euler_problem_0032
