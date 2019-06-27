PROGRAM main
  USE euler_utility, ONLY: li, unit_digit, length_of_int, pop
  IMPLICIT NONE

  INTEGER(li) :: start = 1, RESULT = 0, i, j
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
           RESULT = i
           EXIT
        END IF
        i = i + 1
     END DO
  END DO
  WRITE (*, *) RESULT

CONTAINS
  SUBROUTINE int2arr(n, arr)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li), ALLOCATABLE, DIMENSION(:), INTENT(out) :: arr
    INTEGER(li) :: i, tmp
    ALLOCATE (arr(length_of_int(n))); tmp = n
    DO i = SIZE(arr), 1_li, -1_li
       arr(i) = unit_digit(tmp); tmp = tmp/10
    END DO
  END SUBROUTINE int2arr
  !
  SUBROUTINE swap(a, b)
    IMPLICIT NONE
    INTEGER(li), INTENT(inout) :: a, b
    INTEGER(li) :: tmp
    tmp = a; a = b; b = tmp
  END SUBROUTINE swap
  !
  SUBROUTINE quick_sort(a)
    INTEGER(li), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: a
    INTEGER(li) :: temp
    INTEGER :: i, j
    LOGICAL :: swapped
    DO j = SIZE(a) - 1, 1, -1
       swapped = .FALSE.
       DO i = 1, j
          IF (a(i) > a(i + 1)) THEN
             CALL swap(a(i), a(i + 1))
             swapped = .TRUE.
          END IF
       END DO
       IF (.NOT. swapped) EXIT
    END DO
  END SUBROUTINE quick_sort
  !
  LOGICAL FUNCTION is_permutation(n1, n2)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n1, n2
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: arr1, arr2
    INTEGER(li) :: i, pos
    IF (length_of_int(n1) /= length_of_int(n2)) THEN
       is_permutation = .FALSE.
       RETURN
    END IF
    is_permutation = .FALSE.
    CALL int2arr(n1, arr1); CALL int2arr(n2, arr2)
    CALL quick_sort(arr1); CALL quick_sort(arr2)
    IF (ALL(arr1 == arr2)) is_permutation = .TRUE.
  END FUNCTION is_permutation
END PROGRAM main
