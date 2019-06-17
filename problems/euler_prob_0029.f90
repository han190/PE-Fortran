SUBMODULE(euler_problems) euler_problem_0029
  ! Project Euler: Problem 0029
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0029()
    IMPLICIT NONE
    INTEGER, PARAMETER :: lim = 99
    INTEGER(li), DIMENSION(2, lim, lim) :: array_of_nums
    INTEGER(li), DIMENSION(2, lim*lim) :: arr, res
    INTEGER(li) :: i, j, k
    DO i = 2, lim + 1
       DO j = 2, lim + 1
          array_of_nums(:, j - 1, i - 1) = [j, i]
          CALL simplify_powers(array_of_nums(:, j - 1, i - 1))
       END DO
    END DO
    arr = RESHAPE(array_of_nums, [2, lim*lim])
    res(1:2, 1) = arr(1:2, 1); k = 1_li
    outer: DO i = 2, lim*lim
       inner: DO j = 1, k
          IF (res(1, j) == arr(1, i) .AND. res(2, j) == arr(2, i)) CYCLE outer
       END DO inner
       k = k + 1_li
       res(1:2, k) = arr(1:2, i)
    END DO outer
    WRITE (euler0029, "(i20)") k
  END FUNCTION euler0029
  !
  SUBROUTINE simplify_powers(arr1)
    IMPLICIT NONE
    INTEGER(li), DIMENSION(2), INTENT(inout) :: arr1
    SELECT CASE (arr1(1))
    CASE (4_li, 9_li, 25_li, 36_li, 49_li, 100_li)
       arr1(1) = INT(SQRT(REAL(arr1(1), sp)), li)
       arr1(2) = arr1(2)*2_li
    CASE (8_li)
       arr1(1) = 2_li; arr1(2) = arr1(2)*3_li
    CASE (27_li)
       arr1(1) = 3_li; arr1(2) = arr1(2)*3_li
    CASE (16_li)
       arr1(1) = 2_li; arr1(2) = arr1(2)*4_li
    CASE (81_li)
       arr1(1) = 3_li; arr1(2) = arr1(2)*4_li
    CASE (32_li)
       arr1(1) = 2_li; arr1(2) = arr1(2)*5_li
    CASE (64_li)
       arr1(1) = 2_li; arr1(2) = arr1(2)*6_li
    CASE default
       arr1(1:2) = arr1(1:2)
    END SELECT
  END SUBROUTINE simplify_powers
END SUBMODULE euler_problem_0029
