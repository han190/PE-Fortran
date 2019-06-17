SUBMODULE(euler_problems) euler_problem_0025
  ! Project Euler: Problem 0025
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0025()
    USE euler_utility, ONLY: carry
    IMPLICIT NONE
    INTEGER(li), PARAMETER :: n = 1000_li
    INTEGER(li), DIMENSION(n) :: int_arr1, int_arr2, temp
    INTEGER(li) :: j, k
    int_arr1 = 0_li; int_arr2 = 0_li
    int_arr1(n) = 1_li; int_arr2(n) = 1_li
    k = 3_li
    loop_1: DO
       loop_2: DO j = n, 1_li, -1_li
          temp(j) = int_arr1(j) + int_arr2(j)
       END DO loop_2
       loop_3: DO j = n, 2_li, -1_li
          CALL carry(temp(j - 1:j))
       END DO loop_3
       loop_4: DO j = n, 1_li, -1_li
          int_arr1(j) = temp(j)
       END DO loop_4
       IF (int_arr1(1) /= 0_li) EXIT loop_1
       k = k + 1_li
       loop_5: DO j = n, 1_li, -1_li
          temp(j) = int_arr1(j) + int_arr2(j)
       END DO loop_5
       loop_6: DO j = n, 2_li, -1_li
          CALL carry(temp(j - 1:j))
       END DO loop_6
       loop_7: DO j = n, 1_li, -1_li
          int_arr2(j) = temp(j)
       END DO loop_7
       IF (int_arr2(1) /= 0_li) EXIT loop_1
       k = k + 1_li
    END DO loop_1
    WRITE (euler0025, "(i20)") k
  END FUNCTION euler0025
END SUBMODULE euler_problem_0025
