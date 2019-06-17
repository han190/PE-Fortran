SUBMODULE(euler_problems) euler_problem_0023
  ! Project Euler: Problem 0023
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0023()
    IMPLICIT NONE
    INTEGER(li), PARAMETER :: limit = 28123_li
    INTEGER(li), DIMENSION(limit) :: sums_of_divs, abundant_draft, nums
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: abundant_arr
    INTEGER(li) :: i, j
    LOGICAL, DIMENSION(limit) :: can_be_written
    DO i = 1_li, limit
       DO j = i*2_li, limit, i
          sums_of_divs(j) = sums_of_divs(j) + i
       END DO
    END DO
    abundant_draft = 0_li
    DO i = 1_li, limit
       IF (sums_of_divs(i) > i) abundant_draft(i) = i
    END DO
    ALLOCATE (abundant_arr(COUNT(abundant_draft /= 0_li)))
    abundant_arr = PACK(abundant_draft, abundant_draft /= 0_li)
    can_be_written = .TRUE.
    outer: DO i = 1_li, SIZE(abundant_arr)
       inner: DO j = 1_li, SIZE(abundant_arr)
          ASSOCIATE (x=>abundant_arr(i) + abundant_arr(j))
            IF (x < limit + 1_li) THEN
               can_be_written(x) = .FALSE.
            ELSE
               EXIT inner
            END IF
          END ASSOCIATE
       END DO inner
    END DO outer
    nums = [(i, i=1_li, limit)]
    WRITE (euler0023, "(i20)") SUM(PACK(nums, can_be_written))
  END FUNCTION euler0023
END SUBMODULE euler_problem_0023
