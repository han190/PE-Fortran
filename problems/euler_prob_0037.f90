SUBMODULE(euler_problems) euler_problem_0037
  ! Project Euler: Problem 0037
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0037()
    USE euler_utility, ONLY: is_prime, append, pop
    IMPLICIT NONE
    INTEGER(li) :: sum, cur, push_size, temp, i
    INTEGER(li), DIMENSION(4) :: push_array
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: pre_queue
    sum = 0_li; push_size = 5_li; i = 1_li
    push_array = [1_li, 3_li, 7_li, 9_li]
    ALLOCATE (pre_queue(4)); pre_queue = [2_li, 3_li, 5_li, 7_li]
    outer: DO WHILE (SIZE(pre_queue) > 1_li)
       cur = pre_queue(1)
       CALL pop(pre_queue)
       inner: DO i = 1_li, 4_li
          temp = cur*10_li + push_array(i)
          IF (.NOT. is_prime(temp)) CYCLE inner
          CALL append(pre_queue, temp)
          IF (is_left_truncatable(temp)) sum = sum + temp
       END DO inner
    END DO outer
    WRITE (euler0037, "(i20)") sum
  END FUNCTION euler0037
  !
  LOGICAL FUNCTION is_left_truncatable(n)
    USE euler_utility, ONLY: is_prime
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li) :: temp
    temp = 10_li
    DO WHILE (temp < n)
       IF (.NOT. is_prime(MOD(n, temp))) THEN
          is_left_truncatable = .FALSE.
          RETURN
       END IF
       temp = temp*10_li
    END DO
    is_left_truncatable = .TRUE.
  END FUNCTION is_left_truncatable
END SUBMODULE euler_problem_0037
