SUBMODULE(euler_problems) euler_problem_0052
  ! Project Euler: Problem 0052
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0052()
    IMPLICIT NONE
    INTEGER(li) :: start = 1, ans = 0, i, j
    LOGICAL :: found = .FALSE.
    DO WHILE (.NOT. found)
       start = start*10
       i = start
       DO WHILE (i < start*10/6)
          found = .TRUE.
          DO j = 2, 6
             IF (.NOT. is_permutation(i, j*i)) THEN
                found = .FALSE.; EXIT
             END IF
          END DO
          IF (found) THEN
             ans = i
             EXIT
          END IF
          i = i + 1
       END DO
    END DO
    WRITE (euler0052, "(i20)") ans
  END FUNCTION euler0052
  !
  SUBROUTINE quick_sort(arr)
    USE euler_utility, ONLY: swap
    IMPLICIT NONE
    INTEGER(li), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: arr
    INTEGER(li) :: temp
    INTEGER :: i, j
    LOGICAL :: swapped
    DO j = SIZE(arr) - 1, 1, -1
       swapped = .FALSE.
       DO i = 1, j
          IF (arr(i) > arr(i + 1)) THEN
             CALL swap(arr(i), arr(i + 1))
             swapped = .TRUE.
          END IF
       END DO
       IF (.NOT. swapped) EXIT
    END DO
  END SUBROUTINE quick_sort
  !
  LOGICAL FUNCTION is_permutation(n1, n2)
    USE euler_utility, ONLY: length_of_int, int_2_arr
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n1, n2
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: arr1, arr2
    INTEGER(li) :: i, pos
    IF (length_of_int(n1) /= length_of_int(n2)) THEN
       is_permutation = .FALSE.
       RETURN
    END IF
    is_permutation = .FALSE.
    CALL int_2_arr(n1, arr1); CALL int_2_arr(n2, arr2)
    CALL quick_sort(arr1); CALL quick_sort(arr2)
    IF (ALL(arr1 == arr2)) is_permutation = .TRUE.
  END FUNCTION is_permutation
END SUBMODULE euler_problem_0052
