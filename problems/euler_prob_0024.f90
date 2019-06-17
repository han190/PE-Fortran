SUBMODULE(euler_problems) euler_problem_0024
  ! Project Euler: Problem 0024
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0024()
    IMPLICIT NONE
    INTEGER(ll), DIMENSION(10) :: factor_array
    INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: arr1, arr2
    INTEGER(ll) :: n, temp, i, j, k
    factor_array = [1_ll, 1_ll, 2_ll, 6_ll, 24_ll, 120_ll, 720_ll, &
         5040_ll, 40320_ll, 362880_ll]
    ALLOCATE (arr1(10))
    arr1 = [(i, i=0_ll, 9_ll)]
    n = 999999_ll; temp = 0_ll
    DO i = 10_ll, 1_ll, -1_ll
       j = n/factor_array(i); n = MOD(n, factor_array(i))
       ALLOCATE (arr2(SIZE(arr1) - 1_ll))
       k = arr1(j + 1_ll)
       arr2 = PACK(arr1, arr1 /= k)
       DEALLOCATE (arr1)
       ALLOCATE (arr1(SIZE(arr2)))
       arr1 = arr2
       DEALLOCATE (arr2)
       temp = temp + k*10_ll**(i - 1_ll)
    END DO
    WRITE (euler0024, "(i20)") temp
  END FUNCTION euler0024
END SUBMODULE euler_problem_0024
