SUBMODULE(euler_problems) euler_problem_0043
  ! Project Euler: Problem 0043
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER(ll) :: answer
CONTAINS
  ! The code it optimized for speed so no dynamic arrays or pointers are used.
  ! We don't really have to do this unless performance is the priority.
  MODULE CHARACTER(len=20) FUNCTION euler0043()
    IMPLICIT NONE
    INTEGER(ll), DIMENSION(10) :: test_array
    test_array = [0_ll, 1_ll, 2_ll, 3_ll, 4_ll, 5_ll, 6_ll, 7_ll, 8_ll, 9_ll]
    CALL permute_and_append(10_ll, test_array, 1_ll, 10_ll)
    WRITE (euler0043, "(i20)") answer
  END FUNCTION euler0043
  !
  RECURSIVE SUBROUTINE permute_and_append(soa, a, l, r)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: l, r, soa
    INTEGER(ll), DIMENSION(soa), INTENT(inout) :: a
    INTEGER(ll) :: i, temp
    IF (l == r) THEN
       temp = arr_2_int(soa, a)
       IF (is_subinteger_divisible(temp)) THEN
          answer = answer + temp
       END IF
       RETURN
    ELSE
       DO i = l, r
          CALL swap(a(i), a(l))
          CALL permute_and_append(10_ll, a, l + 1_ll, r)
          CALL swap(a(i), a(l))
       END DO
    END IF
  END SUBROUTINE permute_and_append
  !
  INTEGER(ll) FUNCTION arr_2_int(soa, array)
    IMPLICIT NONE
    INTEGER(ll) :: i, temp, soa
    INTEGER(ll), DIMENSION(soa), INTENT(in) :: array
    temp = array(soa)
    DO i = soa - 1_ll, 1_ll, -1_ll
       temp = temp*10_ll + array(i)
    END DO
    arr_2_int = temp
  END FUNCTION arr_2_int
  !
  SUBROUTINE swap(m, n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(inout) :: m, n
    INTEGER(ll) :: temp
    temp = m; m = n; n = temp
  END SUBROUTINE swap
  !
  LOGICAL FUNCTION is_subinteger_divisible(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll), DIMENSION(7) :: prime_arr
    INTEGER(ll) :: i, temp, three_integer
    prime_arr = [17_ll, 13_ll, 11_ll, 7_ll, 5_ll, 3_ll, 2_ll]; temp = n
    is_subinteger_divisible = .TRUE.
    DO i = 1_ll, 7_ll
       three_integer = temp - temp/1000_ll*1000_ll
       IF (MOD(three_integer, prime_arr(i)) /= 0_ll) THEN
          is_subinteger_divisible = .FALSE.
          RETURN
       END IF
       temp = temp/10_ll
    END DO
  END FUNCTION is_subinteger_divisible
END SUBMODULE euler_problem_0043
