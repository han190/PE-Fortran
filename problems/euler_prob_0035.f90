SUBMODULE(euler_problems) euler_problem_0035
  ! Project Euler: Problem 0035
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0035()
    IMPLICIT NONE
    INTEGER(li) :: i
    INTEGER(li), PARAMETER :: limit = 1000000_li
    LOGICAL, DIMENSION(limit) :: logical_array
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: array
    logical_array = .FALSE.
    DO i = 1_li, limit
       IF (logical_array(i)) CYCLE
       IF (is_circular_prime(i)) THEN
          CALL rotate_integer_array(i, array)
          logical_array(array) = .TRUE.
       END IF
    END DO
    WRITE (euler0035, "(i20)") COUNT(logical_array)
  END FUNCTION euler0035
  !
  INTEGER(li) FUNCTION rotate_integer(n)
    USE euler_utility, ONLY: unit_digit, length_of_int
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    rotate_integer = unit_digit(n)*10_li**(length_of_int(n) - 1_li) + n/10_li
  END FUNCTION rotate_integer
  !
  SUBROUTINE rotate_integer_array(n, array1)
    USE euler_utility, ONLY: length_of_int
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li), ALLOCATABLE, DIMENSION(:), INTENT(out) :: array1
    INTEGER(li) :: i1, temp
    ALLOCATE (array1(length_of_int(n)))
    temp = n; array1(1) = temp
    DO i1 = 2_li, SIZE(array1)
       temp = rotate_integer(temp); array1(i1) = temp
    END DO
  END SUBROUTINE rotate_integer_array
  !
  LOGICAL FUNCTION is_circular_prime(n)
    USE euler_utility, ONLY: is_prime
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: array2
    INTEGER(li) :: i2
    CALL rotate_integer_array(n, array2)
    is_circular_prime = .TRUE.
    DO i2 = 1_li, SIZE(array2)
       IF (.NOT. is_prime(array2(i2))) THEN
          is_circular_prime = .FALSE.
          RETURN
       END IF
    END DO
  END FUNCTION is_circular_prime
END SUBMODULE euler_problem_0035
