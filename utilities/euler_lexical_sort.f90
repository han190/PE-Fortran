! This method is an implementation of an algorithm from:
! http://computer-programming-forum.com/49-fortran/aba487e1d497c041.htm
!-------------------------------------------------------------------------------
SUBMODULE(euler_utility) lexical_sort_module
  IMPLICIT NONE
  INTEGER, DIMENSION(:), ALLOCATABLE :: index_arr
  LOGICAL :: case_sensitive
CONTAINS
  MODULE SUBROUTINE lexical_sort(str_arr, case_insensitive)
    CHARACTER(len=*), DIMENSION(:), INTENT(inout) :: str_arr
    LOGICAL, INTENT(in), OPTIONAL :: case_insensitive
    INTEGER :: low, high, ios, k
    IF (PRESENT(case_insensitive)) THEN
       case_sensitive = .NOT. case_insensitive
    ELSE
       case_sensitive = .TRUE.
    END IF
    low = 1
    high = SIZE(str_arr)
    ALLOCATE (index_arr(high), stat=ios)
    IF (ios /= 0) ERROR STOP "Error allocating index_arr"
    index_arr = (/(k, k=low, high)/)
    CALL quick_sort(str_arr, low, high)
    str_arr = str_arr(index_arr)
    DEALLOCATE (index_arr, stat=ios)
    IF (ios /= 0) ERROR STOP "Error allocating index_arr"
  END SUBROUTINE lexical_sort
  !
  RECURSIVE SUBROUTINE quick_sort(str_arr, low, high)
    CHARACTER(len=*), DIMENSION(:), INTENT(inout) :: str_arr
    INTEGER, INTENT(in) :: low, high
    INTEGER :: pivot_loc
    IF (low < high) THEN
       CALL partition(str_arr, low, high, pivot_loc)
       CALL quick_sort(str_arr, low, pivot_loc - 1)
       CALL quick_sort(str_arr, pivot_loc + 1, high)
    END IF
  END SUBROUTINE quick_sort
  !
  SUBROUTINE partition(str_arr, low, high, pivot_loc)
    CHARACTER(len=*), DIMENSION(:), INTENT(inout) :: str_arr
    INTEGER, INTENT(in) :: low, high
    INTEGER, INTENT(out) :: pivot_loc
    INTEGER :: k, lastsmall
    CALL swap(index_arr(low), index_arr((low + high)/2))
    lastsmall = low
    DO k = low + 1, high
       IF (string_comp(str_arr(index_arr(k)), str_arr(index_arr(low)))) THEN
          lastsmall = lastsmall + 1
          CALL swap(index_arr(lastsmall), index_arr(k))
       END IF
    END DO
    CALL swap(index_arr(low), index_arr(lastsmall))
    pivot_loc = lastsmall
  END SUBROUTINE partition
  !
  FUNCTION string_comp(p, q) RESULT(lexical_less)
    CHARACTER(len=*), INTENT(in) :: p, q
    LOGICAL :: lexical_less
    INTEGER :: kq, k
    IF (case_sensitive) THEN
       lexical_less = p < q
    ELSE
       kq = 1
       DO k = 1, MAX(LEN_TRIM(p), LEN_TRIM(q))
          IF (upper_case(p(k:k)) == upper_case(q(k:k))) THEN
             CYCLE
          ELSE
             kq = k
             EXIT
          END IF
       END DO
       lexical_less = upper_case(p(kq:kq)) < upper_case(q(kq:kq))
    END IF
  END FUNCTION string_comp
  !
  FUNCTION upper_case(letter) RESULT(L)
    CHARACTER(len=*), INTENT(in) :: letter
    CHARACTER(len=1) :: L
    CHARACTER(len=26), PARAMETER :: &
         Lower = "abcdefghijklmnopqrstuvwxyz", &
         Upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    INTEGER :: k
    k = INDEX(Lower, letter)
    IF (k > 0) THEN
       L = Upper(k:k)
    ELSE
       L = letter
    END IF
  END FUNCTION upper_case
END SUBMODULE lexical_sort_module
